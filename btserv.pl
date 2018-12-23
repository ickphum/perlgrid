#!/home/pi/perl5/perlbrew/perls/perl-5.22.1/bin/perl

use strict;
use warnings;

use Log::Log4perl qw(get_logger);
use Getopt::Long;
use Data::Dumper qw(Dumper);
use FindBin qw($Bin);
use List::Util qw(max min);
use English qw(-no_match_vars);
#use File::Slurp;
use File::stat;
#use File::Copy;
#use IO::Prompter;
use Fcntl;
use File::Basename;

use Net::Bluetooth;
use JSON;
use Digest::MD5;

################################################################################

my $log;
my $child_pid = 0;
my $HEADER_START = "LM";
my $HEADER_START_LEN = length($HEADER_START);
my $LENGTH_BYTES = 8;
my $HEADER_LENGTH = 18;
my $CHECKSUM_LENGTH = 32;

################################################################################
sub start_child_process {
    my (@command) = @_;

    if ($child_pid) {
        system('sudo', 'kill', $child_pid);
        $child_pid = 0;
    }

    if ($child_pid = fork()) {
        $log->info("started child $child_pid");
    }
    else {
        my $my_pid = $$;
        close STDOUT or $log->logconfess("Couldn't close stdout");
        close STDERR or $log->logconfess("Couldn't close stderr");
        open(STDOUT, ">/tmp/led_${my_pid}_stdout.log")
            or $log->logconfess("Can't redirect stdout");
        open(STDERR, ">/tmp/led_${my_pid}_stderr.log")
            or $log->logconfess("Can't redirect stderr");
        $log->info("CHILD: running in child $my_pid");
        $log->info("CHILD: command @command");

        exec @command;
    }

    return;
}

################################################################################

unless(caller) {

    # list of options
    my @options = qw(
        man
        usage
        debug
        help
        quiet
        log_dir=s
    );

    my %option;

    GetOptions( \%option, @options ) or die "Usage:?";

    $option{bin_dir} = $Bin;

    $ENV{log_appenders} = $option{quiet} ? 'file' : "file, screen";
    $ENV{log_level}     = $option{debug} ? "DEBUG" : 'INFO';
    $ENV{log_dir}       ||= ($option{log_dir} || $option{bin_dir});
    $ENV{log_file_name} ||= 'btserv';
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

    $log->info("Running $0: " . Dumper(\%option));

    $log->info("create server socket");
    my $server_socket = Net::Bluetooth->newsocket("RFCOMM")
        or die "couldn't create socket : $OS_ERROR";

    $log->info("bind to port 1");
    if ($server_socket->bind(1) != 0) {
        die "couldn't bind to socket : $OS_ERROR";
    }

    $log->info("listen, allowing 1 connection");
    if ($server_socket->listen(1) != 0) {
        die "couldn't listen on socket : $OS_ERROR";
    }

    my $service_obj = Net::Bluetooth->newservice($server_socket, "39827220-b713-11e5-a837-0800200c9a66", "PerlBT", "Perl Bluetooth server")
        or die "couldn't advertise service : $OS_ERROR";

=pod

Protocol for android/pi communications:

* We may have multipart transmissions for a single display sequence;
  we want the stage to revert to the base level whenever a level mismatch
  is found, eg the pi is waiting for a file part and the android has given up
  and restarted.

* most messages will have a single stage; only more complex ones (eg file transmissions) will require more.

* stage + message type identifies the nature of the data in a message, eg FILE stage 2 is the file type.

* structured data (eg several fields) will be sent as a JSON chunk.

* display types for prototype:
  * demo modes, including adhoc image display.

* future display types 
  * scrolling text
  * bouncing ball
  * pong
  * other programmed animations

* Files transferred to the pi are remembered and can be re-used.

* bt server will spawn children to do the display work, so no need for Inline::CPP.

Sample messages:
clear
play demo <d> [ args ]
download & display image
get image list
display old image <n>

Message structure

[2 bytes (ascii)        LM (sanity check)
[8 bytes (hex)]         total message length
[2 bytes (hex)]         stage
[4 bytes (ascii)        message name:
                            OKOK
                            NOPE
                            CLER
                            DEMO
                            FILE
                            DISP
                            LIST
[length bytes]          data
[32 bytes (hex)]        md5 checksum calculated on other parts of message

Examples

clear
  Android: 0/CLER[]
  Pi: 0/OKOK (regardless of state; will reset to state 0)

play demo 0 (no args required)
  A: 0/DEMO[0]
  P: 0/OKOK

play demo 1 (scroll supplied ppm image)

  send file
  A: 0/FILE[<JSON:name, display name, type>]
  P: 0/OKOK
  P: 1/FILE[<data chunk 1>]
  P: 1/OKOK
  P: 1/FILE[<data chunk 2>]
  P: 1/OKOK
  ...
  A: 2/FILE[<size>]
  P: 2/OKOK[<index>]

  play file
  A: 0/DEMO[1]
  P: 0/OKOK
  A: 1/DEMO[1]
  P: 1/OKOK


=cut

    my $incoming_file_name;
    my $incoming_file_length;
    my $out_fh;;

    start_child_process('sudo', "/home/pi/rpi-rgb-led-matrix-master/blinker");

    while (1) {

        # accept a client connection
        $log->info("waiting for client...");
        my $client_obj = $server_socket->accept();
        unless(defined($client_obj)) {
              die "client accept failed: $OS_ERROR\n";
        }

        # get client information
        my ($caddr, $port) = $client_obj->getpeername();
        $log->info("got a client: $caddr, $port");

        # create a Perl filehandle for reading and writing
        my $client_fh = $client_obj->perlfh();

        # a message is made of chunks
        my $reading_message = 1;

        my $data;
        my $bytes_read = 1;

        my $message = '';
        my $message_length = 0;

        while ($bytes_read && ($message_length == 0 || (length $message) < $message_length)) {
#            $log->info("read from client:");
            if ($bytes_read = sysread($client_fh, $data, 20480, 0)) {
                $log->info("read $bytes_read bytes, data length " . length $data);
#                syswrite($client_fh, "[$bytes_read]!");
                $message .= $data;

                # the thing we read is the start token LM then the length of the total message in hex
                if ($message_length == 0 && length $message >= $HEADER_START_LEN + $LENGTH_BYTES) {

                    # TODO send NOPE if header_start != LM
                    my $header_start = substr($message, 0, $HEADER_START_LEN);
                    $log->error("Bad header_start token '$header_start'")
                        unless $header_start eq $HEADER_START;

                    # work out the data length, otherwise we'll keep reading after the data runs out
                    my $length_data = substr($message, $HEADER_START_LEN, $LENGTH_BYTES);
                    $message_length = hex($length_data);
#                    $log->info("message_length $message_length");
                }
            }
        }

        # TODO send NOPE and stop processing if bytes_read < message_length

        # trim to advertised length
        $message = substr($message, 0, $message_length);

        # send back the ack
        $bytes_read = length $message;
        syswrite($client_fh, "LM000AOKOK");

        $log->info("read message $message_length bytes, bytes_read $bytes_read.");

        if (length $message >= $HEADER_LENGTH) {

            # chop off the checksum at the end of the message
            my $checksum = substr($message, -$CHECKSUM_LENGTH, $CHECKSUM_LENGTH, '');

            my $calc_checksum = Digest::MD5::md5_hex($message);
            $log->debug("calc_checksum '$calc_checksum'");
            my $header = substr($message, 0, $HEADER_LENGTH, '');
            $log->debug("header '$header', checksum '$checksum'");

            if ($header =~ /LM([A-F0-9]{8})(\w{4})([A-F0-9]{4})/) {
                my ($name, $attr_len) = ($2,hex($3));
                $log->info("attr len $attr_len");

                my $attribute = {};
                if ($attr_len) {
                    my $json_str = substr($message, 0, $attr_len, '');
                    $attribute = JSON::decode_json($json_str);
                    $log->info("demo attribute: " . Dumper($attribute));
                }

                if ($name eq 'CLER') {
                    $log->info("clear");

                    start_child_process('sudo', "/home/pi/rpi-rgb-led-matrix-master/blinker");
                    $reading_message = 0;
                }
                elsif ($name eq 'DEMO') {

                    start_child_process('sudo', "/home/pi/rpi-rgb-led-matrix-master/led-matrix", "-D", $attribute->{demoNumber});
                    $reading_message = 0;

                    # convert -background black -fill "#ff00ff" -size x32 -gravity center label:"Here we go...   " -depth 8 test.ppm
                }
                elsif ($name eq 'FILE') {
                    $log->info("received file message");
                    $incoming_file_name = basename($attribute->{fileName});

                    # the message is now stripped of header, JSON and checksum,
                    # so it should be the same as the file size
                    my $length = length($message);
                    $log->info("theoretical file length $attribute->{fileLength}, actually received $length");

                    # had some problems with old versions of files hanging around
                    unlink $incoming_file_name if -f $incoming_file_name;

                    sysopen($out_fh, $incoming_file_name, O_WRONLY | O_CREAT)
                        or die "couldn't open $incoming_file_name : $OS_ERROR";

                    syswrite($out_fh, $message, length $message)
                        or die "couldn't write $message_length bytes : $OS_ERROR";

                    close $out_fh;

                    # length matches, start display
                    if ($length == $attribute->{fileLength}) {
                        start_child_process('sudo', "/home/pi/rpi-rgb-led-matrix-master/led-image-viewer", $incoming_file_name);
                    }
                    else {
                        $log->info("length mismatch, remove file"); 
                        unlink $incoming_file_name;
                    }

                }
            }
            else {
                $log->logwarn("bad protocol header: '$header'");
            }
        }
        else {
            $log->logwarn("short message '$message'");
        }

        # close client connection
        $log->info("close connection");
        close($client_fh);
    }

    # stop advertising service
    $service_obj->stopservice();
    # close server connection
    $server_socket->close();

    $log->info("bye");
}
