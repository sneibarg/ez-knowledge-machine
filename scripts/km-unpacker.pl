#!/usr/local/bin/perl
# Splits file with internal file markers of the form:
# ;;; FILE: <filename>
# into individual files in the current directory.
# Outputs to stdout information about processing.
# require 5.0;
$lineno = 0 ;
if ($#ARGV != 0) { die "Usage: $0 filename.";}	# 1 and only 1 arg
$fn = shift(@ARGV);

open(PACKED, "<$fn") || die "Could not open file $fn\n ";
$_ = <PACKED>; $lineno += 1;	# Read first line, and count it
chop;
($junk, $outfile) = split (/:/);

unless ($junk != /^;;; FILE/o) {
	die "Missing file tag ;;; FILE: Line number $lineno."
}
# Open file for writing

unless (open (OUTFILE, ">$outfile")) {
	die "Could not open file $outfile for writing.";
	}
print "$outfile created\n";
while (<PACKED>) {
	$lineno += 1;
	($junk, $outfile) = split (/:/);
	if ($junk =~ /^;;; FILE/o) {
		close (OUTFILE);
		chop($outfile);
		unless (open (OUTFILE, ">$outfile")) {
	die "Could not open file $outfile for writing.  Line number $lineno.";
		}
		print "$outfile created\n";
# uncomment the below line if you want KM files to have KM package declaration
		print (OUTFILE "\n(unless (find-package :km) (make-package :km :use '(:common-lisp)))\n");
		print (OUTFILE "(in-package :km)\n");
	}
	else {
		print (OUTFILE $_);
	}
}
close(PACKED);
close(OUTFILE);
print "Completed without errors. Processed $lineno lines of input from $fn.\n";