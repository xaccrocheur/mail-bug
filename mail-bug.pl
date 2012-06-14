#!/usr/bin/perl

PERL_MM_USE_DEFAULT=1 perl -MCPAN -e 'install Mail::IMAPClient'


# PERL_MM_USE_DEFAULT=1 perl -MCPAN -e 'install Video::Capture::V4l::Imager'

use strict;
use warnings;
use Mail::IMAPClient;
use IO::Socket::SSL;
use MIME::QuotedPrint;
use MIME::Base64;
use Encode qw/encode decode/;
use open OUT => ':utf8';
binmode STDOUT, ":utf8";

my $host = $ARGV[0];
my $port = $ARGV[1];
my $box = $ARGV[2];
my $login = $ARGV[3];
my $pass = $ARGV[4];
my $sep = "|_|";
my $msgid = $ARGV[5] if ( defined $ARGV[5] );

my $number_of_args = $#ARGV + 1;

sub decode_imap_subject {

  my $string=$_[0];
  my ($encoded,$decoded);

  if((defined($string)) and ($string=~/\?(iso-8859|utf|windows)-?[0-9]{0,4}\?/i)) {
    my @encoded_chunks=split(/\s+/,$string);

    foreach my $chunk (@encoded_chunks) {
      $encoded=(split(/\?/,$chunk))[3];
      if($chunk=~/\?(iso-8859-|utf|windows)-?[0-9]{0,4}\?q/i) {
	$decoded.=decode_qp($encoded);
	$decoded=~s/_/ /g;
      }
      elsif($chunk=~/\?(iso-8859-|utf|windows)-?[0-9]{0,4}\?b/i) {
	$decoded.=decode_base64($encoded);
      }
      else {
	$decoded.=$chunk." ";
      }
    }
  }
  else {
    $decoded=$string;
  }
  return $decoded;
}

# We need a better error handling
if ($number_of_args > 4) {
  # Connect to the IMAP server via SSL
  my $socket = IO::Socket::SSL->new(
				    PeerAddr => $host,
				    PeerPort => $port,
				   )
    or die "socket(): $@";

  my $imap = Mail::IMAPClient->new(
				   Socket   => $socket,
				   User     => $login,
				   Password => $pass,
				  )
    or die "new(): $@";

  # print "I'm authenticated\n" if $imap->IsAuthenticated();
  # die;
  # my @folders = $imap->folders();
  # print join("\n* ", 'Folders:', @folders), "\n";

  # foreach my $f ($imap->folders) {
  #   print "The $f folder has ",
  #     $imap->unseen_count($f)||0, " unseen messages.\n";
  # }

  sub escape_quote {
    my $_ = shift;

    s{
       (?<! (?<! \\ ) \\{1} )
       (?<! (?<! \\ ) \\{3} )
       (?<! (?<! \\ ) \\{5} )
       (?<! (?<! \\ ) \\{7} )
       (?= " )
   }{\\}xg;

    return $_;
  }

  $imap->select($box);
  my @mails = ($imap->unseen);

  if ( defined $msgid ) {
    print '(';
    my $from = escape_quote($imap->get_header($msgid, "From"));
    my $date = $imap->get_header($msgid, "Date");
    my $subject = escape_quote(decode('utf8', decode_imap_subject($imap->get_header($msgid, "Subject"))));
    my $body = escape_quote(decode('utf8', decode_qp($imap->body_string($msgid))));

    # print '$from' . '$sep' . '$date' . '$sep' . '$subject' . '$sep' . '$sep' . '$body' . '\n';
    print '(' . '"' . $from . '" "' . $date . '" "' . $subject . '" "' . $body . '")';
    print ')';
  } else {
    print '(';
    foreach my $id (@mails) {
      # my $msgid = $imap->get_header($id, "Message-id");
      my $from = escape_quote($imap->get_header($id, "From"));
      my $date = $imap->get_header($id, "Date");
      my $subject = escape_quote(decode('utf8', decode_imap_subject($imap->get_header($id, "Subject"))));

      print '(' . '"' . $from . '" "' . $date . '" "' . $subject . '" "' . $id . '")';
    }
    print ')';
  }

  # Say bye
  $imap->logout();

} else {
  die("not enough args");
}
