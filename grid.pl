#!/usr/bin/perl -w -- 
#$Id: iso.pl 151 2012-11-11 10:05:50Z ikm $

# modules {{{1

use Wx qw[:everything];

use strict;
use warnings;

use Log::Log4perl qw(get_logger);
use Getopt::Long;
use Data::Dumper qw(Dumper);
use FindBin qw($Bin);
use List::Util qw(max min);
use List::MoreUtils;
use English qw(-no_match_vars);
use POSIX qw(ceil floor);
use Storable;
use Math::Intersection::StraightLine;
use Math::Round;
use Math::Polygon;
use Math::Geometry::Planar;

# main variables {{{1

my $log;

my $Dictionary;

# GridApp {{{1

package GridApp;

use strict;
use warnings;

use Wx qw[:everything wxTheClipboard];
use base qw(Wx::App Class::Accessor::Fast);
use Data::Dumper;
use File::Basename;
use File::Slurp qw(read_file write_file);
use Wx::XRC;
use Wx::DND;
use Digest::SHA qw(sha1_hex);
use Crypt::CBC;
use YAML::XS qw(Dump Load);
use English qw(-no_match_vars);
use POSIX qw(ceil floor);
use List::Util qw(max min);

__PACKAGE__->mk_accessors( qw(frame xrc filename control 
    frame_left _frame_top grid_width grid_height
    current_dir filename
    panel_size panel_led_span led_cell_proportion empty_panel_proportion add_panel_label_width add_panel_label_height
    panel boundary primitives current_panel_key
    frame_count
    color pen brush
    animation_timer
    long_press_timer long_press_data
    bounce_energy_retained
    mouse_panel_x mouse_panel_y mouse_led_x mouse_led_y
    state
    stage
    new_type new_object
    start_point
    ) );

my ($PT_NULL_TYPE,
    $PT_BOX,
    $PT_ELLIPSE,
    $PT_POLYGON,
    $PT_POLYLINE,
    $PT_CURVE,
    $PT_FREELINE,
    ) = (0 .. 10);

# must be in sync with the $PT_* values
my @primitive_type_names = qw(Null_type Box Ellipse Polygon Polyline Curve Freeline);

my ($ST_NORMAL,
    $ST_ADDING,
    $ST_SELECTION,
    $ST_MULTI_SELECTION,
    $ST_MOVING,
    $ST_SIZING,
    ) = (1 .. 10);

sub new { # {{{2
    my( $class, $option ) = @_;
    my $self = $class->SUPER::new();

    die "No main.xrc" unless -f 'main.xrc';

    $self->xrc( Wx::XmlResource->new() );
    $self->xrc->InitAllHandlers;

    $self->xrc->Load('main.xrc');

    $self->frame( $self->xrc->LoadFrame(undef, 'main'));

    $log->info("wxID_NEW " . wxID_NEW);

#    Wx::Event::EVT_MENU($self->frame, wxID_NEW, \&new_file; });
#    Wx::Event::EVT_MENU($self->frame, wxID_OPEN, \&open_file);
    Wx::Event::EVT_MENU($self->frame, wxID_SAVE, \&save_file);
    Wx::Event::EVT_MENU($self->frame, wxID_CLOSE, sub {
        return unless save_file($self->frame);
        $self->frame->Destroy;
    });
#    Wx::Event::EVT_MENU($self->frame, wxID_SAVEAS, \&copy_to_html);
    Wx::Event::EVT_MENU($self->frame, Wx::XmlResource::GetXRCID('tb_box'), sub { prepare_to_add($PT_BOX); } );
    Wx::Event::EVT_MENU($self->frame, Wx::XmlResource::GetXRCID('tb_freeline'), sub { prepare_to_add($PT_FREELINE); } );

    Wx::Event::EVT_MENU($self->frame, Wx::XmlResource::GetXRCID('tb_play'), \&play_animation);
    Wx::Event::EVT_MENU($self->frame, Wx::XmlResource::GetXRCID('tb_step'), sub { $self->advance_animation(1); });
    Wx::Event::EVT_MENU($self->frame, Wx::XmlResource::GetXRCID('tb_pause'), sub { $self->animation_timer->Stop; });
    Wx::Event::EVT_MENU($self->frame, Wx::XmlResource::GetXRCID('tb_trash'), \&delete_selection);
    Wx::Event::EVT_MENU($self->frame, Wx::XmlResource::GetXRCID('tb_stop'), 
        sub { 

            # if we've just stepped, this hasn't been created
            $self->animation_timer->Stop if $self->animation_timer; 

            $self->initialise_objects; 
            $self->advance_animation(0);
        });

    $self->frame->SetAcceleratorTable( Wx::AcceleratorTable->new (
        [ wxACCEL_CTRL, ord('S'), wxID_SAVE ],
        [ wxACCEL_CTRL, ord('W'), wxID_CLOSE ],
#        [ wxACCEL_CTRL, ord('D'), wxID_HELP ],
#        [ wxACCEL_CTRL, ord('F'), wxID_FIND ],
#        [ wxACCEL_CTRL, ord('R'), wxID_REPLACE ],
#        [ wxACCEL_ALT,  ord('D'), $REFRESH_DICTIONARY_ID ],
    ));

    Wx::Event::EVT_SIZE($self->frame, sub {
        my ($frame, $event) = @_;
        
        my $size = $event->GetSize;

        $event->Skip;
    });

    Wx::Event::EVT_MOVE($self->frame, sub {
        my ($frame, $event) = @_;
        
        $event->Skip;
    });

    Wx::Event::EVT_CLOSE($self->frame, sub {
        my ($frame, $event) = @_;

        if ($self->check_for_changes) {
            $frame->Destroy;
        }
        else {
            $event->Veto;
        }
    });

    $self->control({});
    my $record_objects;
    $record_objects = sub {
        my ($parent) = @_;

        for my $child ( $parent->GetChildren ) {
            $log->info("child $child " . $child->GetName);
            $self->control->{ $child->GetName } = $child;
            $record_objects->($child);
        }
    };
    $record_objects->($self->frame);

    main::assign_event_handler($self->control->{grid_pnl}, 'EVT_PAINT', \&grid_paint);
    main::assign_event_handler($self->control->{grid_pnl}, 'EVT_SIZE', 
        sub { 
            my ($panel, $event) = @_; 
            my ($w, $h) = $panel->GetClientSizeWH;
            wxTheApp->grid_width($w);
            wxTheApp->grid_height($h);
            $panel->Refresh; 
            $event->Skip;
        });
    main::assign_event_handler($self->control->{grid_pnl}, 'EVT_MOTION', \&grid_mouse_motion);
    main::assign_event_handler($self->control->{grid_pnl}, 'EVT_LEFT_DOWN', \&grid_mouse_left_down);
    main::assign_event_handler($self->control->{grid_pnl}, 'EVT_LEFT_UP', \&grid_mouse_left_up);

    $self->SetTopWindow($self->frame);
    $self->frame->Show(1);
    $self->frame->Move([100,100]);
    $self->current_dir($option->{bin_dir});

    my %color = (
        border              => '#404040',
        panel               => '#4f4f4f',
        current_panel       => '#234F1D',
        led                 => '#888888',
        hilite_led          => '#aaaaaa',
        grid                => '#4f4f4f',
        empty_border        => '#d0d0d0',
        empty_panel         => '#b0b0b0',
        start_point         => '#8af39b',
        corner_highlight    => '#20e14445', # 45% alpha
        edge_highlight      => '#10752345',
        centre_highlight    => '#08391145',
    );

    $self->color({});
    $self->pen({});
    $self->brush({});
    for my $color_name (keys %color) {
        if ($color{$color_name} =~ /#(..)(..)(..)(..)/) {
            my ($red, $green, $blue, $alpha) = (hex($1),hex($2),hex($3), int(($4/100)*255));
            $log->info("rgba = $red, $green, $blue, $alpha");
            $self->color->{$color_name} = Wx::Colour->new($red, $green, $blue, $alpha);
            $log->info("alpha " . $self->color->{$color_name}->Alpha);
        }
        else {
            $self->color->{$color_name} = Wx::Colour->new($color{$color_name});
        }
        $log->logdie("color not created for $color_name") unless $self->color->{$color_name} && $self->color->{$color_name}->IsOk;
        $self->pen->{$color_name} = Wx::Pen->new($self->color->{$color_name}, $color_name eq 'border' ? 2 : 1, wxSOLID);
        $self->brush->{$color_name} = Wx::Brush->new($self->color->{$color_name}, wxSOLID);
    }

    my $imagelist = Wx::ImageList->new( 16, 16, 1 );
    for my $image_stub (map { lc $_ } @primitive_type_names) {
        $imagelist->Add(Wx::Bitmap->new("image/$image_stub.png", wxBITMAP_TYPE_ANY));
    }
    my $primitive_tcl = $self->control->{primitive_tcl};
    $primitive_tcl->AssignImageList($imagelist);
    my $root = $primitive_tcl->AddRoot('root', $PT_NULL_TYPE);
    Wx::Event::EVT_TREE_SEL_CHANGED($primitive_tcl, $primitive_tcl, sub {
        my ($tree, $event) = @_;

        # the selection list is trustworthy but nothing else is; we have
        # to reset all selection states based on the list.
        map { $_->{selected} = 0; } @{ wxTheApp->primitives };
        for my $item_id ($tree->GetSelections) {
            my $data = $tree->GetPlData($item_id);
            $data->{primitive}->{selected} = 1;
            $log->info("selection: " . $data->{primitive}->{name});
        }

        my $app = wxTheApp;
        $log->info("tree selection");
        $app->advance_animation(0);
        $app->control->{grid_pnl}->Refresh;

        return;
    });

    Wx::Event::EVT_TREE_END_LABEL_EDIT($primitive_tcl, $primitive_tcl, sub {
        my ($tree, $event) = @_;

        my $item_id = $event->GetItem;
        my $data = $tree->GetPlData($item_id);

        $event->Skip;

        # comes back as blank when you click on the same item
        return unless my $new_label = $event->GetLabel;

        # check for duplicates
        for my $primitive (@{ wxTheApp->primitives }) {
            if ($primitive->{name} eq $new_label) {
                if ($primitive == $data->{primitive}) {
                    $log->info("ignore match on same object");
                }
                else {
                    Wx::MessageBox("Duplicate name '$new_label'");
                    $event->Veto;
                    return;
                }
            }
        }

        # change the name 
        $data->{primitive}->{name} = $event->GetLabel;

        return;
    });

    $self->long_press_timer(Wx::Timer->new($self->frame, -1));
    Wx::Event::EVT_TIMER($self->frame, $self->long_press_timer, sub {
        my ($frame, $event) = @_;
        $log->info("long press timer @_");
        $event->Skip;

        # set or clear current_panel_key
        $self->current_panel_key($self->current_panel_key eq $self->long_press_data ? '' : $self->long_press_data);
        $self->control->{grid_pnl}->Refresh;

        # if we've just selected a panel, clear object selection(s)
        # (only necessary in multiselect since the mouse down event
        # clears selection otherwise)
        map {
            $_->{selected} = 0;
            $primitive_tcl->UnselectItem($_->{tree_item_id});
        } grep { $_->{selected} } @{ wxTheApp->primitives };

        return;
    });

    $self->panel_size(320);
    $self->panel_led_span(32);
    $self->led_cell_proportion(0.75);
    $self->empty_panel_proportion(0.8);
    $self->bounce_energy_retained(0.95);
    $self->mouse_panel_x(-1);
    $self->mouse_panel_y(-1);
    $self->mouse_led_x(-1);
    $self->mouse_led_y(-1);
    $self->current_panel_key('');

    if ($option->{file}) {
        open_file($self->frame, undef, $option);
    }
    else {

        $self->primitives([]);

        # hard-coded primitives {{{
        $self->primitives([
            {
                type => $PT_BOX,
                left => 10,
                bottom => 12,
                width => 8,
                height => 4,
                rotation => {
                    degrees => 5,
#                    around_centroid => 1,
#                    centre_offset => [ 2, 2 ],
#                    centre => [ 14, 50 ],
                },
                velocity => [ 2, 1 ],
                gravity_effect => 0,
#                paths => [
#                    {
#                        speed => 0.5,
#                        points => [
#                            [ 15, 43 ],
#                            [ 19, 49 ],
#                            [ 26, 44 ],
#                            [ 28, 40 ],
#                            [ 28, 40 ],
#                            [ 32, 35 ],
#                            [ 38, 30 ],
#                        ],
#                    },
#                    {
#                        speed => 2,
#                        points => [
#                            [ 44, 25 ],
#                            [ 46, 20 ],
#                            [ 47, 18 ],
#                            [ 47, 16 ],
#                            [ 44, 13 ],
#                            [ 40, 12 ],
#                            [ 36, 13 ],
#                            [ 33, 15 ],
#                            [ 33, 18 ],
#                            [ 34, 24 ],
#                            [ 38, 28 ],
#                        ],
#                    },
#                ],
            },
            {
                type => $PT_POLYGON,
                left => 40,
                bottom => 5,
                points => [ 
                    [ 2, 0 ],
                    [ 0, 2 ],
                    [ 0, 4 ],
                    [ 2, 6 ],
                    [ 4, 6 ],
                    [ 6, 4 ],
                    [ 6, 2 ],
                    [ 4, 0 ],
                ],
                velocity => [ 5,4 ],
            },
    #        {
    #            type => $PT_BOX,
    #            left => 1,
    #            bottom => 5,
    #            width => 4,
    #            height => 3,
    #            velocity => [ 2,1 ],
    #        },
    #        {
    #            type => $PT_BOX,
    #            left => 1,
    #            bottom => 9,
    #            width => 4,
    #            height => 3,
    #            velocity => [ 2,1 ],
    #        },
    #        {
    #            type => $PT_BOX,
    #            left => 10,
    #            bottom => 22,
    #            width => 12,
    #            height => 1,
    #        },
        ]);
#        #}}}
#
        # add panel at 0,0
        $self->panel({
            '0,0' => {},
            '1,0' => {},
            '0,1' => {},
            '1,1' => {},
        });
    }

    $self->find_panel_boundary;
    $self->reset_state;

    # initialise LEDs for starting positions
    $self->initialise_objects;

    # display starting positions
    $self->advance_animation(0);

    return $self;
}

################################################################################
sub grid_mouse_motion { #{{{2
    my ($grid, $event) = @_;

    my $app = wxTheApp;
    my ($x, $y) = ($event->GetX, $app->grid_height - $event->GetY - 1);

    # any movement stops the long press timer
    $app->long_press_timer->Stop;

    my $panel_size = $app->panel_size;
    my $panel_led_span = $app->panel_led_span;
    my $panel_x = $app->mouse_panel_x(int($x / $panel_size));
    my $panel_y = $app->mouse_panel_y(int($y / $panel_size));
#    my $led_x = $app->mouse_led_x(int(($x - ($panel_x * $panel_size))/($panel_size / $panel_led_span)));
#    my $led_y = $app->mouse_led_y(int(($y - ($panel_y * $panel_size))/($panel_size / $panel_led_span)));
    my $led_x = $app->mouse_led_x;
    my $led_y = $app->mouse_led_y;
    my $new_led_x = $app->mouse_led_x(int($x/($panel_size / $panel_led_span)));
    my $new_led_y = $app->mouse_led_y(int($y/($panel_size / $panel_led_span)));
    if ($app->state == $ST_ADDING && ($new_led_x != $led_x || $new_led_y != $led_y)) {
        $log->info("refresh during add");
        if ($app->stage == 0) {
            $grid->Refresh;
        }
        else {
            $app->update_new_object;
        }
    }

    # $log->info("grid_mouse_motion $x,$y, panel $panel_x,$panel_y, led $led_x, $led_y");

    $event->Skip;
    return;
}

################################################################################
sub grid_mouse_left_down { #{{{2
    my ($grid, $event) = @_;

    my $app = wxTheApp;
    my $refresh;

    my $canvas_tbr = $app->control->{canvas_tbr};
    my $multiselect = $canvas_tbr->GetToolState(Wx::XmlResource::GetXRCID('tb_multiselect'));
    $log->info("multiselect $multiselect");

    if ($app->state == $ST_ADDING && $app->stage == 0) {
        $app->start_point([$app->mouse_led_x, $app->mouse_led_y]);
        $app->stage(1);
        $app->update_new_object;
    }
    elsif ($app->state == $ST_NORMAL) {

        # selection {{{3
        # cope with:
        # selecting an object
        # selecting an existing grid (ie not on an object)
        # clicking on an 'add this button' area

        my ($panel_x, $panel_y) = ($app->mouse_panel_x, $app->mouse_panel_y);
        my $panel_key = "$panel_x,$panel_y";

        if (my $panel = $app->panel->{$panel_key}) {

            # state of current LED?
            my ($led_x, $led_y) = ($app->mouse_led_x, $app->mouse_led_y);
            $led_x -= $app->panel_led_span * $panel_x;
            $led_y -= $app->panel_led_span * $panel_y;
            my $selected_object;
            if (my $led_state = $panel->{led_state}->[$led_x]->[$led_y]) {
#                $log->info("led state: " . Dumper($led_state));

                # toggle selection the object for this LED, if this LED has a single object.
                # uniq is used because a single LED will frequently be part of 2 sides for
                # the same object, ie at corners. Avoiding this would be trivial for boxes but
                # much less so for other types of object.
                if (scalar(List::MoreUtils::uniq @{ $led_state->{objects} }) == 1) {
                    $log->info("toggle selection");
                    $selected_object = $led_state->{objects}->[0];
                    $selected_object->{selected} = $selected_object->{selected} ? 0 : 1;
                    $app->control->{primitive_tcl}->ToggleItemSelection($selected_object->{tree_item_id});

                    # any change to object selection clears the panel selection
                    $app->current_panel_key('');

                    $refresh = 1;
                }
                else {
                    $log->info("multiple objects at this point");
                }

            }
            else {
#                $log->info("no led state at $led_x, $led_y on panel $panel_key : " . Dumper($panel->{led_state}));
                $log->info("no led state at $led_x, $led_y on panel $panel_key");

                # start long-press timer for panel selection
                $app->long_press_timer->Start(300, wxTIMER_ONE_SHOT);
                $app->long_press_data($panel_key);
            }

            # if multiselect is not set, any selection action clears the selection
            # of any items other than the one possibly selected above
            unless ($multiselect) {
                for my $object (@{ $app->primitives }) {
                    next if $selected_object && $object == $selected_object;
                    next unless $object->{selected};
                    $object->{selected} = 0;
                    $app->control->{primitive_tcl}->UnselectItem($object->{tree_item_id});
                }
            }
        }
        else {

            # not a current panel; are we in the button section?
            $log->info("new panel?");
            my ($x, $y) = ($event->GetX, $event->GetY);
            my $panel_size = $app->panel_size;
            my $grid_x_coord = $panel_x * $panel_size + 1;
            my $grid_y_coord = $app->grid_height - $panel_y * $panel_size - $panel_size;

            my ($box_width, $box_height) = ($app->add_panel_label_width, $app->add_panel_label_height);
            my $left_edge = $grid_x_coord + $panel_size / 2 - $box_width / 2;
            my $top_edge = $grid_y_coord + $panel_size / 2 - $box_height / 2;
            $log->info("mouse at $x,$y box at $left_edge, $top_edge dim $box_width x $box_height");
            if ($x >= $left_edge && $x <= $left_edge + $box_width && $y >= $top_edge && $y <= $top_edge + $box_height) {
                $log->info("in box");

                # add this panel
                $app->panel->{$panel_key} = {};
                $app->find_panel_boundary;
                $app->advance_animation(0);
                $refresh = 1;
            }
        }

        #}}}
    }
    else {
        $app->reset_state;
    }

    $log->info("done grid_mouse_left_down");

    $app->control->{grid_pnl}->Refresh if $refresh;

    return;
}

################################################################################
sub grid_mouse_left_up { #{{{2
    my ($grid, $event) = @_;
    $log->info("grid_mouse_left_up");

    my $app = wxTheApp;
#    my ($x, $y) = ($event->GetX, $app->grid_height - $event->GetY - 1);

    # button up stops long press timer
    $app->long_press_timer->Stop;

    if ($app->state == $ST_ADDING && $app->stage == 1) {
        $app->reset_state;
    }

    return;
}

################################################################################
sub reset_state { #{{{2
    my ($app) = @_;

    $log->info("reset state");
    $app->state($ST_NORMAL);
    $app->stage(0);
    $app->new_object(undef);
    $app->new_type(undef);
    $app->control->{grid_pnl}->Refresh;

    return;
}

################################################################################
sub prepare_to_add { #{{{2
    my ($type) = @_;

    my $app = wxTheApp;

    if ($app->state == $ST_ADDING) {
        $app->reset_state;
        return;
    }

    return unless $app->state == $ST_NORMAL;

    $app->state($ST_ADDING);
    $app->stage(0);
    $app->new_type($type);

    return;
}

################################################################################
sub add_primitive { #{{{2
    my ($app, $new_object) = @_;

    push @{ $app->primitives }, $new_object;

    $new_object->{name} ||= $primitive_type_names[$new_object->{type}] . ' ' . $#{ $app->{primitives} };

    my $primitive_tcl = $app->control->{primitive_tcl};

    $new_object->{tree_item_id} = $primitive_tcl->AppendItem(
        $primitive_tcl->GetRootItem, 
        $new_object->{name}, 
        $new_object->{type}, 
        -1, 
        Wx::TreeItemData->new({ primitive => $new_object }),
    );

    if ($new_object->{selected}) {
        $primitive_tcl->SelectItem($new_object->{tree_item_id});
    }

    return;
}

################################################################################
sub update_new_object { #{{{2
    my ($app) = @_;

    $log->info("update_new_object from " . Dumper($app->start_point) . " to " . $app->mouse_led_x . ',' . $app->mouse_led_y);

    my ($start_x, $start_y) = @{ $app->start_point };
    my ($mouse_x, $mouse_y) = ($app->mouse_led_x, $app->mouse_led_y);

    my $new_object;
    unless ($new_object = $app->new_object) {
        $new_object = {
            type => $app->new_type,
            rotation => {
                degrees => 45,
#                around_centroid => 1,
#                centre_offset => [ 2, 2 ],
                centre => [ 14, 50 ],
            },
            velocity => [ 2, 2 ],
        };
        $app->new_object($new_object);
        $app->add_primitive($new_object);
    }

    if ($app->new_type == $PT_BOX) {
        $log->info("update new box");
        $new_object->{left} = min($start_x, $mouse_x);
        $new_object->{bottom} = min($start_y, $mouse_y);
        $new_object->{width} = abs($start_x - $mouse_x) + 1;
        $new_object->{height} = abs($start_y - $mouse_y) + 1;
    }
    elsif ($app->new_type == $PT_FREELINE) {
        $log->info("update new freeline");

        unless ($new_object->{points}) {
            $new_object->{left} = $start_x;
            $new_object->{bottom} = $start_y;
            $new_object->{points} = [];
            $new_object->{open} = 1;
        }

        my $previous_point = $#{$new_object->{points} } > -1
            ? $new_object->{points}->[ $#{ $new_object->{points} } ]
            : undef;

        # points are stored as offsets from start so we can move the line
        $mouse_x -= $new_object->{left};
        $mouse_y -= $new_object->{bottom};
        if (! defined $previous_point || ($previous_point->[0] != $mouse_x && $previous_point->[1] != $mouse_y)) {
            push @{ $new_object->{points} }, [ $mouse_x, $mouse_y ];
        }
    }
    else {
        $log->warn("Unknown type " . $app->new_type);
        return;
    }

    my $vertices = create_initial_vertices($new_object);
#    $log->info("new vertices " . Dumper($vertices));
#    render_led_lines($new_object, $vertices);
    $app->advance_animation(0);

    return;
}

################################################################################
sub delete_selection { #{{{2
    my ($frame) = @_;
    my $app = wxTheApp;

    if (length $app->current_panel_key) {
        delete $app->panel->{ $app->current_panel_key };
        $app->current_panel_key('');
        $app->find_panel_boundary;
    }

    $app->control->{grid_pnl}->Refresh;

    return;
}

################################################################################
sub find_panel_boundary { #{{{2
    my ($app) = @_;

    # find panel extents (min & max x for all rows)
    my %row;
    for my $panel_key (keys %{ $app->panel }) {
        if ($panel_key =~ /([-0-9]+),([-0-9]+)/) {
            my ($x,$y) = ($1,$2);
            if ($row{$y}) {
                $row{$y}->[0] = min($row{$y}->[0], $x);
                $row{$y}->[1] = max($row{$y}->[1], $x);
            } 
            else {
                $row{$y} = [
                    $x,
                    $x,
                ];
            }
        }
        else {
            $log->logdie("Panel '$panel_key' is malformed");
        }
    }
    $log->debug("row: " . Dumper(\%row));

    # start at bottom left; this is first point on boundary. Record cx & cy.
    my $min_row = min(keys %row);
    my $max_row = max(keys %row);
    my $panel_led_span = $app->panel_led_span;
    my ($current_col,undef) = @{ $row{$min_row} };
    my @vertices = ( [$current_col * $panel_led_span, $min_row * $panel_led_span ] ); 
    my @corners = ([-1,-1]);

    # process each row in turn looking at min x & max x
    my $current_row = $min_row + 1;

    while ($current_row <= $max_row) {
        $log->debug("try row $current_row");
        if ($row{$current_row}) {
            my ($min_col, $max_col) = @{ $row{$current_row} };
            $log->debug("row from $min_col to $max_col");

            # if min x <> current x, add vertices at current x and min x, both at the bottom of the current row
            if ($min_col != $current_col) {
                
                # whether the coord falls in the current panel or one out of it depends 
                # on the direction of the turn
                my $y = $current_row * $panel_led_span;
                $y-- if $min_col > $current_col;

                push @vertices, 
                    [ $current_col * $panel_led_span, $y ],
                    [ $min_col * $panel_led_span, $y ];
                push @corners, $min_col > $current_col
                    ? ( [ -1, 1 ], [ -1, 1 ] )
                    : ( [ -1, -1 ], [ -1, -1 ] );
                $current_col = $min_col;
            }

        }

        $current_row++;
    }

    # after top row done, add vertex at min x and max x at top of row
    my ($min_col, $max_col) = @{ $row{$max_row} };
    push @vertices, 
        [ $min_col * $panel_led_span, ($max_row + 1) * $panel_led_span - 1],
        [ ($max_col + 1) * $panel_led_span - 1, ($max_row + 1) * $panel_led_span - 1];
    push @corners, [ -1, 1 ], [ 1, 1 ];

    $current_row = $max_row - 1;
    $current_col = $max_col;

    # repeat process from top row back to bottom row, looking at max x instead.
    while ($current_row >= $min_row) {
        if ($row{$current_row}) {
            my ($min_col, $max_col) = @{ $row{$current_row} };

            # if max x <> current x, add vertices at current x and max x, both at the top of the current row
            if ($max_col != $current_col) {

                # see above
                my $y = ($current_row + 1) * $panel_led_span;
                $y-- if $max_col > $current_col;
                push @vertices, [ ($current_col + 1) * $panel_led_span - 1, $y ],
                    [ ($max_col + 1) * $panel_led_span - 1, $y ];
                push @corners, $max_col > $current_col
                    ? ( [ 1, 1 ], [ 1, 1 ] )
                    : ( [ 1, -1 ], [ 1, -1 ] );
                $current_col = $max_col;
            }


        }

        $current_row--;
    }

    # after bottom row done, add vertices at max x at bottom of row and back at start again to close polygon
    ($min_col, $max_col) = @{ $row{$min_row} };
    push @vertices, [ ($max_col + 1) * $panel_led_span - 1, $min_row * $panel_led_span ],
        [$min_col * $panel_led_span, $min_row * $panel_led_span ]; 
    push @corners, [ 1, -1 ];

    $log->debug("vertices " . Dumper(\@vertices, \@corners));

    $app->boundary({
        polygon => Math::Polygon->new(@vertices),
        corner_signs => \@corners,
    });

    return;
}

################################################################################
sub play_animation { #{{{2

    my $app = wxTheApp;
    unless ($app->animation_timer) {
        $app->animation_timer(Wx::Timer->new($app->frame, -1));

        my $handler = sub {
            return unless $app->advance_animation(1);
            $app->animation_timer->Start(50, wxTIMER_ONE_SHOT);
            return;
        };

        Wx::Event::EVT_TIMER($app->frame, $app->animation_timer, $handler);

    }
    my $animation_timer = $app->animation_timer;

    $animation_timer->Start(50, wxTIMER_ONE_SHOT);

    return;
}

################################################################################
sub initialise_objects { #{{{2
    my ($app) = @_;

    for my $object (@{ $app->primitives }) {
        create_initial_vertices($object);
        delete $object->{live};
        if (exists $object->{velocity}) {
            $object->{live}->{velocity} = [ $object->{velocity}->[0], $object->{velocity}->[1] ];
        }
        if (exists $object->{rotation}) {
            $object->{live}->{rotation} = Storable::dclone($object->{rotation});
            if (exists $object->{rotation}->{centre_offset}) {
                $object->{live}->{rotation}->{offset_centre} = [
                    $object->{rotation}->{centre_offset}->[0] + $object->{vertices}->[0]->[0],
                    $object->{rotation}->{centre_offset}->[1] + $object->{vertices}->[0]->[1],
                ];
            }
        }
        if (exists $object->{paths}) {
            $object->{live}->{path}->{path_index} = 0;
            $object->{live}->{path}->{next_path_point} = 0;
            $object->{live}->{path}->{path_location} = [ $object->{vertices}->[0]->[0], $object->{vertices}->[0]->[1] ];
        }
    }

    $app->frame_count(0);

    return;
}

################################################################################
sub create_initial_vertices { #{{{2
    my ($object) = @_;

    my $vertices = $object->{vertices} = [];

    if ($object->{type} == $PT_BOX) {

        # boxes have 4 corners
        push @{ $vertices }, [ $object->{left}, $object->{bottom}, ],
            [ $object->{left}, $object->{bottom} + $object->{height} - 1, ],
            [ $object->{left} + $object->{width} - 1, $object->{bottom} + $object->{height} - 1, ],
            [ $object->{left} + $object->{width} - 1, $object->{bottom} ];
    }
    elsif ($object->{type} == $PT_POLYGON || $object->{type} == $PT_POLYLINE || $object->{type} == $PT_FREELINE) {
        push @{ $vertices }, map { [ $_->[0] + $object->{left}, $_->[1] + $object->{bottom} ] } @{ $object->{points} };
    }
    else {
        $log->logdie("unknown type $object->{type}");
    }

    # build-stage transformations

    return $vertices;
}

################################################################################
sub render_led_lines { #{{{2
    my ($object, $vertices) = @_;

    # render the lines between the vertices
    for my $index ( 0 .. $#{ $vertices } ) {

        my $on_final_vertex = $index == $#{ $vertices };

        # most objects are closed, but lines and curves aren't ie we don't draw from last to 0
        last if $on_final_vertex && $object->{open};

        my $next_index = $on_final_vertex
            ? 0
            : $index + 1;

        # with collision detection, all lines should be drawn ok
        unless (line($vertices->[$index]->[0], $vertices->[$index]->[1], $vertices->[$next_index]->[0], $vertices->[$next_index]->[1], \&draw_panel_led, $object))
        {
            $log->warn("line couldn't be drawn " . Dumper($vertices->[$index],$vertices->[$next_index]));
        }
    }

    if ($object->{selected}) {
        $log->info("selected object: " . Dumper($object));
    }

    return;
}

################################################################################
sub line { #{{{2
    my ($from_y, $from_x, $to_y, $to_x, $callback, $object) = @_;
    $_ = int $_ for ($from_y, $from_x, $to_y, $to_x);
    my ($delta_y, $delta_x) = ($to_y-$from_y, $to_x-$from_x);
    my $dir = abs($delta_y) > abs($delta_x);
    my ($curr_maj, $curr_min, $to_maj, $to_min, $delta_maj, $delta_min) = $dir
        ? ($from_y, $from_x,  $to_y, $to_x,  $delta_y, $delta_x)
        : ($from_x, $from_y,  $to_x, $to_y,  $delta_x, $delta_y);

    # find signs (1 = +ve, 0 = 0,-1 = -ve) for deltas
    my $inc_maj = $delta_maj ? abs($delta_maj) == $delta_maj ? 1 : -1 : 0;
    my $inc_min = $delta_min ? abs($delta_min) == $delta_min ? 1 : -1 : 0;

    ($delta_maj, $delta_min) = (abs($delta_maj)+0, abs($delta_min)+0);
    my $d = (2 * $delta_min) - $delta_maj;
    my $d_inc1 = $delta_min * 2;
    my $d_inc2 = ($delta_min - $delta_maj) * 2;

    {
        my @point = $dir
            ? ($curr_maj, $curr_min)
            : ($curr_min, $curr_maj);
        $callback->($object, @point) or return 0;

        last if $curr_maj == $to_maj;
        $curr_maj += $inc_maj;
        if ($d < 0) {
            $d        += $d_inc1;
        }
        else {
            $d        += $d_inc2;
            $curr_min += $inc_min;
        }
        redo;
    }
    return 1;
}

################################################################################
# mark the LED point as lit on the appropriate panel
sub draw_panel_led { #{{{2
    my ($object, $x,$y) = @_;

    my $app = wxTheApp;

    my $grid_x = floor($x / $app->panel_led_span);
    my $grid_y = floor($y / $app->panel_led_span);

#    my $panel = ($app->panel->{"$grid_x,$grid_y"} ||= {
#        led_state => [],
#    });
    my $panel = $app->panel->{"$grid_x,$grid_y"} or return 0;

    # convert to led indexes within this panel
    my $led_x = $x - $grid_x * $app->panel_led_span;
    my $led_y = $y - $grid_y * $app->panel_led_span;
    if ($led_x < 0 || $led_y < 0) {
        $log->logconfess("led -ve; $x,$y $grid_x, $grid_y, $led_x, $led_y");
    }

    my $rgb = $object->{selected}
        ? $object->{type} == $PT_BOX
            ? 0xff0000
            : 0x00ff00
        : 0xff0000;
    $panel->{led_state}->[$led_x]->[$led_y] ||= {
        objects => [],
        rgb     => 0,
    };
    $panel->{led_state}->[$led_x]->[$led_y]->{rgb} |= $rgb;
    push @{ $panel->{led_state}->[$led_x]->[$led_y]->{objects} }, $object;

    return 1;
}

################################################################################
# update all object positions by the given time interval, and also set the led
# states for the affected panels
sub advance_animation { #{{{2
    my ($app, $t_sec) = @_;

    return unless $app->panel;

    # clear all panel LEDs; only those marked by the new object positions should be lit
    for my $panel (values %{ $app->panel }) {
        $panel->{led_state} = [];
    }

    my $frame_count = $app->frame_count($app->frame_count + 1);
    $log->info("frame $frame_count");

    # slow down on bounce
    my $bounce_energy_retained = $app->bounce_energy_retained;

    my $panel_led_span = $app->panel_led_span;
    my $finder = Math::Intersection::StraightLine->new();

#    my $result = $finder->points([
#          [
#            63,
#            0
#          ],
#          [
#            63,
#            31
#          ],
#        ],
#        [
#          [
#            63,
#            '14.25'
#          ],
#          [
#            67,
#            '13.25'
#          ]
#        ]);
#    $log->info("test result $result");
#    return 0;

    my $boundary = $app->boundary;
    my $boundary_polygon = $boundary->{polygon};

    OBJECT:
    for my $object (@{ $app->primitives }) {

        # create the generic vertex list from the type-specific definition.
        # Vertices should go clockwise.
        my $vertices = $object->{vertices} || create_initial_vertices($object);

        if ($t_sec) {

            # rotation {{{3
            if (my $rotation = $object->{live}->{rotation}) {

                my $polygon = Math::Polygon->new(@{ $object->{vertices} }, $object->{vertices}->[0]);

                my $center = $rotation->{around_centroid}
                    ? $polygon->centroid
                    : ($object->{live}->{rotation}->{centre} 
                        || $object->{live}->{rotation}->{offset_centre} 
                        || $object->{vertices}->[0]);

                if (my $degrees = $rotation->{degrees}) {

                    my $new_poly = $polygon->rotate(center => $center, degrees => $degrees);
                    my $outside;
                    for my $new_vertex ($new_poly->points) {
                        unless ($boundary_polygon->contains($new_vertex)) {
                            $outside = 1;
                            last;
                        }
                    }
                    unless ($outside) {
                        $vertices = $object->{vertices} = [ @{ $new_poly->points } ];
                    }

                }
            }
            #}}}

            # velocity {{{3
            if (my $velocity = $object->{live}->{velocity}) {

                # change the live object definition (ie the vertex list) according to the time elapsed

                my $velocity_sign = [
                    abs($velocity->[0]) ? $velocity->[0] / abs($velocity->[0]) : 0,
                    abs($velocity->[1]) ? $velocity->[1] / abs($velocity->[1]) : 0,
                ];

                # if we're starting out on a boundary and heading into it, we can change the velocity
                # before we start running through the points
                my $found_collinear_x;
                my $found_collinear_y;
                VERTEX:
                for my $index (0 .. $#{ $vertices }) {

                    # check collinearity
                    for my $corner_index (0 .. $boundary_polygon->order - 1) {
                        my $vertex = $vertices->[$index];
                        my $corner = $boundary_polygon->point($corner_index);
                        my $next_corner = $boundary_polygon->point($corner_index + 1);

                        # if point is on one of the corner's coords but not the other, but the other coord has to fall
                        # in the range for the side.
#                        $log->info("vertex, corner, next_corner : " . Dumper($vertex, $corner, $next_corner));
                        if (($vertex->[0] == $corner->[0] 
                                && $vertex->[1] > min($corner->[1], $next_corner->[1])
                                && $vertex->[1] < max($corner->[1], $next_corner->[1])

                                # on a vertical border; check the x velocity is into the border
                                && ($corner->[1] < $next_corner->[1] ? $velocity->[0] < 0 : $velocity->[0] > 0))
                            || ($vertex->[1] == $corner->[1] 
                                && $vertex->[0] > min($corner->[0], $next_corner->[0])
                                && $vertex->[0] < max($corner->[0], $next_corner->[0])
                                && ($corner->[0] < $next_corner->[0] ? $velocity->[1] > 0 : $velocity->[1] < 0)))
                        {

                            # collinear; hack for straight edges, just see which way the edge runs
                            if ($vertex->[0] == $corner->[0]) {
                                $found_collinear_x = 1;
                                $log->info("collinear on vertical boundary, switch X velocity");
                            } 
                            elsif ($vertex->[1] == $corner->[1]) {
                                $found_collinear_y = 1;
                                $log->info("collinear on horizontal boundary, switch Y velocity");
                            } 

                            # last VERTEX;
                        }
                    }
                }

                # if we were collinear on one axis, switch on that basis. If we're collinear on two, we must be
                # on a corner.
                if ($found_collinear_x xor $found_collinear_y) {

                    # force low Y velocity to 0; turn tiny bounces into rolling
                    if ($found_collinear_y) {
                        if (abs($velocity->[1]) > 0 && abs($velocity->[1]) < 0.5) {
                            $log->info("start rolling");
                            $object->{live}->{rolling} = 1;
                            $velocity->[1] = 0;
                        }
                    }

                    $velocity->[$found_collinear_x ? 0 : 1] *= -1 * $bounce_energy_retained;
                    $log->info("new velocity after bounce " . Dumper($velocity));
                }
                else {

                    # check unique corners
                    VERTEX:
                    for my $index (0 .. $#{ $vertices }) {

                        my $vertex = $vertices->[$index];
                        for my $corner_index (0 .. $boundary_polygon->order - 1) {
                            my $corner = $boundary_polygon->point($corner_index);

                            # point must be on a corner and the velocity direction must match the corner's type
                            if ($corner->[0] == $vertex->[0] && $corner->[1] == $vertex->[1]
                                && $boundary->{corner_signs}->[$corner_index]->[0] == $velocity_sign->[0]
                                && $boundary->{corner_signs}->[$corner_index]->[1] == $velocity_sign->[1])
                            {
                                
                                # exact match for corner; change velocity by corner reflection
                                $log->info("corner match");
                                my $swap = $velocity->[0];
                                $velocity->[0] = -1 * $bounce_energy_retained * $velocity->[1];
                                $velocity->[1] = -1 * $bounce_energy_retained * $swap;
                                last VERTEX;
                            }
                        }
                    }
                }

                # before we transform, save the vertex list in case we need to reset in case
                # of collision
                my $saved_vertices = Storable::dclone($vertices);

                # translation; 

                # * change the vertices in the object according to the current velocity
                # * if the point exceeds current panel boundary, find the intersection of the boundary and
                #   and the line joining the 2 points (old and new). Record the transform between the 
                #   original point and the intersection; this is the transform that will place the point
                #   directly on the boundary, ie at the limit of rendering.
                # * Once all points have been transformed, if there are no exceeding points, we're done.
                # * If there are exceptions, we need to find the limiting point, ie the point that would
                #   first touch the side. This should be min(abs(vx)+abs(vy)). 
                # * We then reset the object and apply the limiting transform to all points. We also
                #   change the velocity according to the elastic properties of the boundary.

                # change the vertices in the object according to the current velocity
                my $min_border_velocity;
                for my $index ( 0 .. $#{ $vertices } ) {
                    my $vertex = $vertices->[$index];
                    $vertex->[0] += Math::Round::nearest(0.01, $velocity->[0]);
                    $vertex->[1] += Math::Round::nearest(0.01, $velocity->[1]);

                    # if the new point is outside the boundary...
                    unless ($boundary_polygon->contains($vertex)) {
                        my $orig_vertex = $saved_vertices->[$index];
#                        $log->info("point is now outside boundary " . Dumper($orig_vertex, $vertex));

                        # find which side we crossed
                        for my $corner_index (0 .. $boundary_polygon->order - 1) {
                            my $corner = $boundary_polygon->point($corner_index);
                            my $next_corner = $boundary_polygon->point($corner_index + 1);

                            if (my $result = $finder->point_limited([ $corner, $next_corner ], [ $orig_vertex, $vertex ])) {

                                next unless ref $result;

                                # this is the boundary we crossed and we assume we can't cross two
#                                $log->info("corner index $corner_index has a result " . Dumper($result));

                                # we have a valid intersection; find the velocity vector from the original point to
                                # the intersection so we can tell which vertex hit the border first (smallest vector wins)
                                $result->[0] = Math::Round::round($result->[0]);
                                $result->[1] = Math::Round::round($result->[1]);
                                my $border_velocity = [ $result->[0] - $orig_vertex->[0], $result->[1] - $orig_vertex->[1], 

                                    # inspect the ends of the border to determine how velocity is changed
                                    $corner->[0] == $next_corner->[0] ? -1 : 1,
                                    $corner->[0] == $next_corner->[0] ? 1 : -1,
                                ];
                                if (! defined $min_border_velocity 
                                    || (abs($border_velocity->[0]) + abs($border_velocity->[1])) < (abs($min_border_velocity->[0]) + abs($min_border_velocity->[1])))
                                {
                                    $min_border_velocity = $border_velocity;
#                                    $log->info("new min_border_velocity " . Dumper($min_border_velocity));
                                }

                                last;
                            }
                        }

                    }
                }

                # did at least 1 vertex cross a border?
                if ($min_border_velocity) {

                    # yes, so we have to reset the vertex list and apply the border velocity
#                    $log->info("min_border_velocity " . Dumper($min_border_velocity));
                    map {
                        $vertices->[$_]->[0] = $saved_vertices->[$_]->[0] + $min_border_velocity->[0]; 
                        $vertices->[$_]->[1] = $saved_vertices->[$_]->[1] + $min_border_velocity->[1]; 
                    } (0 .. $#{ $vertices });

                    # change the velocity to bounce away from the border and maybe lose some magnitude
                    $velocity->[0] *= $min_border_velocity->[2] * $bounce_energy_retained;
                    $velocity->[1] *= $min_border_velocity->[3] * $bounce_energy_retained;

                    # move the relative centre of rotation by the min velocity.
                    # Since the centre of rotation is never affected by rotation by definition,
                    # once we've established via the initial offset, all we need do is apply any
                    # translations.
                    if ($object->{live}->{rotation}->{offset_centre}) {
                        $object->{live}->{rotation}->{offset_centre}->[0] += $min_border_velocity->[0];
                        $object->{live}->{rotation}->{offset_centre}->[1] += $min_border_velocity->[1];
                    }
                }
                else {

                    # move the relative centre of rotation by the velocity
                    if ($object->{live}->{rotation}->{offset_centre}) {
                        $object->{live}->{rotation}->{offset_centre}->[0] += $velocity->[0];
                        $object->{live}->{rotation}->{offset_centre}->[1] += $velocity->[1];
                    }
                }

                if ($object->{live}->{rolling}) {

                    # friction
                    if (abs($velocity->[0]) > 0.1) {
                        $log->info("friction");
                        $velocity->[0] *= 0.5;
                    }
                    else {
                        $log->info("stop");
                        delete $object->{live}->{velocity};
                    }
                }
                else {

                    # gravity
                    $velocity->[1] -= 0.2 * (exists $object->{gravity_effect} ? $object->{gravity_effect} : 1);
#                    $log->info("new velocity after gravity " . Dumper($velocity));
                }
            }
            #}}}

            # follow path {{{3
            if (my $path_info = $object->{live}->{path}) {

                # looking at path_info, we're at path_location heading towards next_path_point on path path_index.
                my $path = $object->{paths}->[ $path_info->{path_index} ];

                my $distance = $path->{speed};
                while ($distance > 0) {

                    # find remaining length in current segment, ie from path_location to next_path_point.
                    $log->info("path : " . Dumper(
                        $path_info->{path_location},
                        $path->{points}->[ $path_info->{next_path_point} ],
                        $path_info,
#                        $path,
                    ));
                    my $next_point = $path->{points}->[ $path_info->{next_path_point} ];
                    my $segment_length = Math::Geometry::Planar::SegmentLength([
                        $path_info->{path_location}, $next_point ]);
                    $distance = Math::Round::nearest(0.1, $distance - $segment_length);
                    $log->info("segment_length $segment_length, distance now $distance");
                    if ($distance >= 0) {

                        # we've got at least as far as the next point, so move location to there.
                        # Path points are invariant so we can just assign it
                        $path_info->{path_location} = $path->{points}->[ $path_info->{next_path_point} ];
                        $log->info("move to next point, new location " 
                            . Dumper($path_info->{path_location}));

                        # did we have enough speed to move further?
                        if ($distance > 0) {

                            $log->info("advance down path");

                            # is there a next point on the current path?
                            if ($path_info->{next_path_point} < $#{ $path->{points} }) {
                                $log->info("move to next point");
                                $path_info->{next_path_point}++;
                            }

                            # no? how about more paths?
                            elsif ($path_info->{path_index} < $#{ $object->{paths} }) {
                                $log->info("move to next path");
                                $path_info->{path_index}++;

                                # note that we don't change speed to that of the new path
                                $path = $object->{paths}->[ $path_info->{path_index} ];
                                $path_info->{next_path_point} = 0;
                            }
                            else {

                                # ok, no more path
                                $log->info("stop");
                                delete $object->{live}->{path};
                                last;
                            }
                        }
                    }
                    else {

                        # we can't get to the next point so move as far as we can towards it.

                        # get the extent of travel back again; note that $distance < 0 here
                        my $segment_proportion = ($segment_length + $distance) / $segment_length;
                        $path_info->{path_location}->[0] += ($next_point->[0] - $path_info->{path_location}->[0]) * $segment_proportion;
                        $path_info->{path_location}->[1] += ($next_point->[1] - $path_info->{path_location}->[1]) * $segment_proportion;
                        $log->info("move towards point; segment_proportion $segment_proportion, new location " 
                            . Dumper($path_info->{path_location}));
                            
                    }
                }

                # we've found the new location along the path; translate the object there. We don't care about borders,
                # if a path is supplied we trust it.
                my $path_move = [ map { $path_info->{path_location}->[$_] - $vertices->[0]->[$_] } (0,1) ];
                $log->info("path move " . Dumper($path_move));
                map {
                    $vertices->[$_]->[0] += $path_move->[0];
                    $vertices->[$_]->[1] += $path_move->[1];
                } (0 .. $#{ $vertices });

            }
            #}}}
        }

        render_led_lines($object, $vertices);
        

    }

    # $log->info("panels : " . Dumper($app->panel));

    $app->control->{grid_pnl}->Refresh;

    return 1;
}

################################################################################
sub grid_paint { #{{{2
    my( $self, $event ) = @_;

    my $app = wxTheApp;
    
    my $dc = Wx::PaintDC->new( $self );

    my ($width, $height) = ($self->GetClientSize->x, $self->GetClientSize->y);
#    $log->info("paint");

    # draw all visible panel locations, either as real panels or as place markers
    my $panels_across = int($width / $app->panel_size);
    my $panels_up = int($height / $app->panel_size);
    # $log->info("width = $width, height = $height => panels_across $panels_across, panels_up $panels_up");

    my $panel_size = $app->panel_size;
    my $empty_panel_size = $panel_size * $app->empty_panel_proportion;
    my $empty_panel_offset = ($panel_size - $empty_panel_size) / 2;
    my $panel_led_span = $app->panel_led_span;
    my $mouse_led_x = $app->mouse_led_x;
    my $mouse_led_y = $app->mouse_led_y;
    my $start_point = $app->start_point;
    my $state = $app->state;
    my $stage = $app->stage;

    my $add_panel_text = 'Add This Panel';
    my ($box_width, $box_height);
    my ($text_width, $text_height, undef, undef) = $dc->GetTextExtent($add_panel_text, $dc->GetFont);
    if ($app->add_panel_label_width) {
        ($box_width, $box_height) = ($app->add_panel_label_width, $app->add_panel_label_height);
    }
    else {

        # work out the box dims once only and save; seems like overkill but the mouse handler needs these as well
        ($box_width, $box_height) = ($text_width * 1.3, $text_height * 1.4);
        $app->add_panel_label_width($box_width);
        $app->add_panel_label_height($box_height);
    }

    my $led_cell_size = $panel_size / $panel_led_span;
    my $led_radius = ($led_cell_size * $app->led_cell_proportion) / 2;

    # draw panels and LEDs {{{
    for my $grid_x (0 .. $panels_across) {
        for my $grid_y (0 .. $panels_up) {

            my $grid_x_coord = $grid_x * $panel_size + 1;
            my $grid_y_coord = $height - $grid_y * $panel_size - $panel_size;
            my $grid_led_offset_x = $grid_x * $panel_led_span;
            my $grid_led_offset_y = $grid_y * $panel_led_span;

            my $panel_key = "$grid_x,$grid_y";
            if (my $panel = $app->panel->{$panel_key}) {

                # draw this panel
                $dc->SetBrush($panel_key eq $app->current_panel_key
                    ? $app->brush->{current_panel}
                    : $app->brush->{panel});
                $dc->SetPen($app->pen->{border});
                $dc->DrawRectangle( $grid_x_coord, $grid_y_coord, $panel_size, $panel_size );

                # draw LEDs from the bottom up; led coords are for the centre of the circle
                my $led_y_coord = $grid_y_coord + $panel_size - $led_cell_size / 2;

                $dc->SetPen(wxTRANSPARENT_PEN);

                for my $led_row (0 .. $panel_led_span - 1) {

                    # my initial col offset is half a cell, in from left edge
                    my $led_x_coord = $grid_x * $panel_size + $led_cell_size / 2;

                    my $grid_led_y = $grid_led_offset_y + $led_row;

                    for my $led_col (0 .. $panel_led_span - 1) {

                        my $grid_led_x = $grid_led_offset_x + $led_col;

                        my $color = 'led';
                        if (my $led_state = $panel->{led_state}->[$led_col]->[$led_row]) {

                            $color = '#' . sprintf('%06x', $led_state->{rgb});

                            # it might look better having this in the generic code (with SetBrush)
                            # but in actuality only the LED colors might not have brushes; checking
                            # for the existence of the system brushes would be a small waste.
                            unless ($app->brush->{$color}) {
                                $app->brush->{$color} = Wx::Brush->new(Wx::Colour->new($color), wxSOLID);
                            }
                        }
                        elsif ($state == $ST_ADDING) {
                            if ($stage == 0) {
                                if ($grid_led_x == $mouse_led_x || $grid_led_y == $mouse_led_y) {
                                    $color = 'start_point';
                                }
                            }
                            elsif ($stage == 1) {
                                if ($grid_led_x == $start_point->[0] || $grid_led_y == $start_point->[1]) {
                                    $color = 'start_point';
                                }
                                elsif ($grid_led_x == $mouse_led_x || $grid_led_y == $mouse_led_y) {
                                    $color = 'hilite_led';
                                }
                            }
                        }

                        $dc->SetBrush($app->brush->{$color});
                        $dc->DrawCircle($led_x_coord, $led_y_coord, $led_radius);
                        $led_x_coord += $led_cell_size;
                    }

                    # move up for the next row
                    $led_y_coord -= $led_cell_size;
                }

            }
            else {

                # draw a place marker
                $dc->SetBrush($app->brush->{empty_panel});
                $dc->SetPen($app->pen->{empty_border});
                $dc->DrawRectangle( $grid_x_coord + $empty_panel_offset, $grid_y_coord + $empty_panel_offset, $empty_panel_size, $empty_panel_size );

                $dc->DrawRectangle( $grid_x_coord + $panel_size / 2 - $box_width / 2, $grid_y_coord + $panel_size / 2 - $box_height / 2, $box_width, $box_height);
                $dc->DrawText($add_panel_text, $grid_x_coord + $panel_size / 2 - $text_width / 2, $grid_y_coord + $panel_size / 2 - $text_height / 2);
            }

        }
    }
    #}}}

    # draw and record selection handles; we have to use a graphics context to
    # have transparency work.
    my $gdc = Wx::GraphicsContext::Create($dc);
    my $handle_size = $led_cell_size * 2;
    for my $object (grep { $_->{selected} } @{ $app->{primitives} }) {

        # selection handles are dependent on object type
        if ($object->{type} == $PT_BOX) {

            my $vertices = $object->{vertices};

            # we want to draw 4 corner handles and if space permits,
            # 4 side handles between the corner handles.

            $gdc->SetBrush($app->brush->{corner_highlight});
            $gdc->SetPen($app->pen->{corner_highlight});
            my @offsets = (
                [ -1, 1 ],
                [ -1, 2 ],
                [ 0, 2 ],
                [ 0, 1 ],
            );
            my $v = 0;
            for my $vertex (@{ $vertices }) {
                $gdc->DrawRectangle( $led_cell_size * ($vertex->[0] + $offsets[$v]->[0]), 
                    $height - ($led_cell_size * ($vertex->[1] + $offsets[$v]->[1])), 
                    $handle_size, $handle_size);
                $v++;
            }

            $gdc->SetBrush($app->brush->{edge_highlight});
            $gdc->SetPen($app->pen->{edge_highlight});
            if ($object->{width} > 2) {
                $gdc->DrawRectangle( $led_cell_size * ($vertices->[1]->[0] + 1), $height - ($led_cell_size * ($vertices->[1]->[1] + 2)), 
                    ($object->{width} - 2) * $led_cell_size, $handle_size);
                $gdc->DrawRectangle( $led_cell_size * ($vertices->[0]->[0] + 1), $height - ($led_cell_size * ($vertices->[0]->[1] + 1)), 
                    ($object->{width} - 2) * $led_cell_size, $handle_size);
            }
            if ($object->{height} > 2) {
                $gdc->DrawRectangle( $led_cell_size * ($vertices->[1]->[0] - 1), $height - ($led_cell_size * ($vertices->[1]->[1])), 
                    $handle_size, ($object->{height} - 2) * $led_cell_size);
                $gdc->DrawRectangle( $led_cell_size * ($vertices->[2]->[0]), $height - ($led_cell_size * ($vertices->[2]->[1])), 
                    $handle_size, ($object->{height} - 2) * $led_cell_size);
            }

            # if either dimension <= 3, draw the centre handle to cover the LEDs,
            # otherwise cover the gap inside them.
            $gdc->SetBrush($app->brush->{centre_highlight});
            $gdc->SetPen($app->pen->{centre_highlight});
            if ($object->{width} <= 3 || $object->{height} <= 3) {
                $gdc->DrawRectangle( $led_cell_size * ($vertices->[1]->[0]), $height - ($led_cell_size * ($vertices->[1]->[1] + 1)), 
                    $object->{width} * $led_cell_size, $object->{height} * $led_cell_size);
            }
            else {
                $gdc->DrawRectangle( $led_cell_size * ($vertices->[1]->[0] + 1), $height - ($led_cell_size * ($vertices->[1]->[1])), 
                    ($object->{width} - 2) * $led_cell_size, ($object->{height} - 2) * $led_cell_size);
            }
        }
    }

#    my $boundary_polygon = $app->boundary->{polygon};
#    my $points = $boundary_polygon->points;
#    $dc->DrawPolygon($points, 0, 0);
    $event->Skip;
    
    return;
}

################################################################################
sub frame_top { #{{{2
    my ($self, $top) = @_;

    if (defined $top) {
        $self->_frame_top($top);
        return $top;
    }
    else {
        my $real_top = $self->_frame_top;
        return $real_top + 27;
    }
}

################################################################################
# Just clear the text control and the filename and key attributes
sub new_file { #{{{2
    my ($no_check) = @_;

    my $app = wxTheApp;
    unless ($no_check) {
        return unless $app->check_for_changes;
    }

    return;
}

################################################################################
sub save_file { #{{{2
    my ($frame, $event) = @_;

    my $app = wxTheApp;

    unless ($app->filename) {

        my $file_dialog = Wx::FileDialog->new($frame, "Choose a filename", $app->current_dir, '', 'Grid files|*.grid|All files|*', wxFD_SAVE);
        return unless $file_dialog->ShowModal == wxID_OK;

        my $filename = $file_dialog->GetPath;
        $filename .= '.grid' unless $filename =~ /\./;

        if (-f $filename) {
            return unless wxYES == Wx::MessageBox("File '$filename' exists; ok to overwrite?", "Confirm Overwrite", wxYES_NO, $frame);
        }
        $app->current_dir($file_dialog->GetDirectory);
        $app->filename( $filename );
    }

    my $filename = $app->filename;

    my ($width, $height) = $frame->GetSizeWH;
    my ($left, $top) = $frame->GetPositionXY;

    # 

    my $yaml = Dump({
        left => $left,
        top => $top,
        width => $width,
        height => $height,
        primitives => $app->primitives,
        panel => [ keys %{ $app->panel } ],
    });

    my $file_text = $yaml;

    write_file($filename, { binmode => ':raw' }, \$file_text);

    $log->info("save to $filename");

    return $filename;
}

################################################################################
sub open_file { #{{{2
    my ($frame, $event, $option) = @_;

    my $app = wxTheApp;
    return unless $app->check_for_changes;

    my $filename = $option->{file};

    unless ($filename) {

        my $file_dialog = Wx::FileDialog->new($frame, "Choose a file to open", $app->current_dir, '', 'Grid files|*.grid|All files|*', wxFD_OPEN | wxFD_FILE_MUST_EXIST);
        return unless $file_dialog->ShowModal == wxID_OK;
        $filename = $file_dialog->GetPath;
        $app->current_dir($file_dialog->GetDirectory);
    }

    $log->debug("open from $filename");
    $app->filename($filename);

    my $file_text = read_file($filename, binmode => ':raw');

    my $yaml = Load($file_text);

    $app->primitives([]);
    for my $object ( @{ $yaml->{primitives} } ) {
        $app->add_primitive($object);
    }

    $app->panel({ map { $_ => {} } @{ $yaml->{panel} } });
#    $frame->SetSize(@{ $yaml }{qw(left top width height)});
#    $frame->SetSize(100,800,400,400);
#    $frame->Move([@{ $yaml }{qw(left top)}]);
#    $frame->Move([ 400,800 ]);

    return;
}

################################################################################
sub check_for_changes { #{{{2
    my ($self) = @_;

    return 1;

#    return $checksum eq $self->saved_checksum
#        ? 1
#        : wxYES == Wx::MessageBox("Ok to lose changes?", "Lose Changes", wxYES_NO, $self->frame);
}

################################################################################
sub OnInit { # {{{2
    my( $self ) = shift;

    Wx::InitAllImageHandlers();

    my $rc = $self->SUPER::OnInit();
    $log->debug("class init $rc");

    return 1;
}

################################################################################

sub OnExit { # {{{2
    my( $self ) = shift;

    return 1;
}

################################################################################

# main functions {{{1
package main;

################################################################################
sub assign_event_handler { #{{{2
    my ($control, $event, $handler) = @_;

#    $log->debug("handle $event for " . $self->name);

    my $event_type = "Wx::Event::$event";

    # find out how many args the event type needs to assign the handler
    my $arg_count = length prototype($event_type);

    my @controls = ($control);
    if ($arg_count == 3) {

        # 3 arg events need the parent as the first arg
        unshift @controls, $control->GetParent;
    }
    elsif ($arg_count == 4) {

        # the 4 arg version is used for handlers which affect a range of controls;
        # not modelled yet
        $log->logdie("no 4 arg events yet");
    }
    elsif ($arg_count != 2) {
        $log->logdie("bad event arg $arg_count");
    }

    # assign the handler
    {
        no strict 'refs';
        &{ $event_type }(@controls, $handler);
    }
}

# mainline {{{1

unless(caller) {

    # list of options
    my @options = qw(
        man
        usage
        debug
        file=s
        key=s
        quiet
        geometry=s
        script=s
    );

    my %option;

    GetOptions( \%option, @options ) or pod2usage(2);
    pod2usage(2) if $option{usage};
    pod2usage(1) if $option{help};
    pod2usage( -exitstatus => 0, -verbose => 2 ) if $option{man};

    # put this in %option
    $option{bin_dir} = $Bin;

    $ENV{log_appenders} = $option{quiet} ? 'file' : "file, screen";
    $ENV{log_level}     = $option{debug} ? "DEBUG" : 'INFO';
    $ENV{log_dir}       ||= $option{bin_dir};
    $ENV{log_file_name} ||= 'grid';
    my $default_config = << "EOT" ;
log4perl.rootLogger=$ENV{log_level}, $ENV{log_appenders}

log4perl.appender.file=Log::Dispatch::FileRotate
log4perl.appender.file.filename=$ENV{log_dir}/$ENV{log_file_name}.log
# log4perl.appender.file.filename=/tmp/$ENV{log_file_name}.log
log4perl.appender.file.umask=0000
log4perl.appender.file.mode=append
log4perl.appender.file.layout=PatternLayout
log4perl.appender.file.size=10000000
log4perl.appender.file.max=3
log4perl.appender.file.layout.ConversionPattern=%d{MM-dd HH:mm:ss} [%p] %F{1} %L - %m%n

log4perl.appender.screen=Log::Log4perl::Appender::Screen
log4perl.appender.screen.layout=PatternLayout
log4perl.appender.screen.layout.ConversionPattern=%d{HH:mm:ss} [%p] %F{1} %L - %m%n
EOT

    my $log_config_file = $option{bin_dir} . '/log4perl.conf';
    if (-f $log_config_file) {
        Log::Log4perl->init( $log_config_file );
    }
    else {
        Log::Log4perl->init( \$default_config );
    }
    $log = get_logger();

    $log->debug("Running $0: " . Dumper(\%option));

    my $app = GridApp->new(\%option);

    $app->MainLoop();

}

################################################################################

__END__

=head1

TODO

Everything

=cut
