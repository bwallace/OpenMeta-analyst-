#!/bin/env perl

BEGIN {
use File::Basename;
unshift(@INC, dirname $0);
}

use strict;
use Spreadsheet::ParseExcel;

# declare some varibles local
my($row, $col, $sheet, $cell, $usage, $basename, $sheetnumber, $filename);

##
## Usage information
##
$usage = <<EOF;

xls2csv.pl <excel file> [<output file>] [<worksheet number>]

Translate the Microsoft Excel spreadsheet file contained in
<excel file> into comma separated value format (CSV) and store
in <output file>.

If <output file> is not specified, the output file will have the
same name as the input file with '.xls' or '.XLS' (if any)
removed and '.csv' appended.

If no worksheet number is given, each worksheet will be written to
a separate file with the name '<output file>_<worksheet name>.csv'.

EOF

##
## parse arguments
##

if(!defined($ARGV[0]))
  {
    print $usage;
    exit 1;
  }

$basename = $ARGV[1];
$basename =~ s/.csv//;
if ($basename eq "")
  {
    my @path;
    @path = split(/[\/\\]/, $ARGV[0]); # split on file separator
    $basename =  $path[$#path];
    $basename =~ s/.xls//i;
  }

if(defined($ARGV[2]) )
  {
    $sheetnumber = $ARGV[2];
    die "Sheetnumber must be an integer larger than 0." if $sheetnumber < 1;
  }

##
## open spreadsheet
##

my $oExcel = new Spreadsheet::ParseExcel;

print "Loading $ARGV[0] ...\n";

open(FH, "<$ARGV[0]") or die "Unable to open file '$ARGV[0]'.\n";
close(FH);

my $oBook = $oExcel->Parse($ARGV[0]);

print "\n";
print "Orignal Filename :", $oBook->{File} , "\n";
print "Number of Sheets :", $oBook->{SheetCount} , "\n";
print "Author           :", $oBook->{Author} , "\n";
print "\n";

my @sheetlist =  (@{$oBook->{Worksheet}});
if (defined($sheetnumber))
  {
    @sheetlist=($sheetlist[$sheetnumber-1]);
  }

##
## iterate across each worksheet, writing out a separat csv file
##

my $i=0;
foreach my $sheet (@sheetlist)
{
  $i++;

  my $sheetname = $sheet->{Name};
  if(defined($sheetnumber))
    {
      $filename = "${basename}.csv";
    }
  else
    {
      $filename = "${basename}_${sheetname}.csv";
    }

  print "Writing Sheet number $i ('$sheetname') to file '$filename'\n";

  open(OutFile,">$filename");

  my $cumulativeBlankLines=0;

  my $minrow = $sheet->{MinRow};
  my $maxrow = $sheet->{MaxRow};
  my $mincol = $sheet->{MinCol};
  my $maxcol = $sheet->{MaxCol};

  print "Minrow=$minrow Maxrow=$maxrow Mincol=$mincol Maxcol=$maxcol\n";

  for(my $row =  $minrow; $row <= $maxrow; $row++)
    {
       my $outputLine = "";

       for(my $col = $mincol; $col <= $maxcol; $col++)
         {
           my $cell = $sheet->{Cells}[$row][$col];
	   if( defined($cell) )
	      {
		$_=$cell->Value; #{Val};

		# convert '#NUM!' strings to missing (empty) values
		s/#NUM!//;

		# escape double-quote characters in the data since
		# they are used as field delimiters
		s/\"/\\\"/g;
	      }
	   else 
	     {
	       $_ = '';
	     }

	   $outputLine .= "\"" . $_ . "\"" if(length($_)>0);

	   # separate cells with commas
	   $outputLine .= "," if( $col != $maxcol) ;

         }

       #$outputLine =~ s/[, ]+$//g;  ## strip off trailing blanks and commas

       # skip blank/empty lines
       if( $outputLine =~ /^[, ]*$/ )
	 {
	   $cumulativeBlankLines++
	 }
       else
	 {
	   print OutFile "$outputLine \n"
	 }
     }

  close OutFile;

  print "  (Ignored $cumulativeBlankLines blank lines.)\n" 
    if ($cumulativeBlankLines);
  print "\n";
}

