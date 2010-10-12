#-*- mode: perl; perl-indent-level: 4; cperl-indent-level: 4 -*-

# Copyright (C) 1997-2009 R Development Core Team
#
# This document is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available at
# http://www.r-project.org/Licenses/

use File::Basename;
use Cwd;
use Getopt::Long;
use R::Rdconv;
use R::Rdlists;
use R::Utils;
use R::Dcf;

fileparse_set_fstype; # Unix, in case one gets anything else.

my $revision = ' $Rev: 47888 $ ';
my $version;
my $name;

$revision =~ / ([\d\.]*) /;
$version = $1;
($name = $0) =~ s|.*/||;

## switch on autoflushing for STDOUT.  We want this as we
## write to both STDERR (warnings) and STDOUT.
$| = 1;

@knownoptions = ("rhome:s", "html", "txt", "latex", "example", "chm",
		 "debug|d", "help|h", "version|v", "os|OS:s");
GetOptions (@knownoptions) || usage();
&R_version($name, $version) if $opt_version;
&usage() if $opt_help;

$OSdir ="unix";
$OSdir = $opt_os if $opt_os;

$WINDOWS = ($OSdir eq "windows");

$dir_mod = 0755;#- Permission ('mode') of newly created directories.

my $current = cwd();
if($opt_rhome){
    $R_HOME=$opt_rhome;
    print STDERR "R_HOME from --rhome: '$R_HOME'\n" if $opt_debug;
} elsif($ENV{"R_HOME"}) {
    $R_HOME=$ENV{"R_HOME"};
    print STDERR "R_HOME from ENV: '$R_HOME'\n" if $opt_debug;
} else {
    die "R_HOME is not set\n"
}

$R_HOME =~ s+\\+/+g; # Unix-style path

chdir($current);
print STDERR "Current directory (cwd): '$current'\n" if $opt_debug;

my $mainlib = file_path($R_HOME, "library");


# default is to build all documentation formats
if(!$opt_html && !$opt_txt && !$opt_latex && !$opt_example && !$opt_chm){
    $opt_html = 1;
    $opt_txt = 1;
    $opt_latex = 1;
    $opt_example = 1;
    $opt_chm = 1;
}
$opt_chm = 0 unless $WINDOWS;

($pkg, $version, $lib, @mandir) = buildinit();
$dest = $ARGV[2];
if (!$dest) {$dest = file_path($lib, $pkg);}

print STDERR "Destination dest = '$dest'\n" if $opt_debug;

my $def_encoding = "unknown";
if(-r &file_path($dest, "DESCRIPTION")) {
    my $rdcf = R::Dcf->new(&file_path($dest, "DESCRIPTION"));
    if($rdcf->{"Encoding"}) {
	    $def_encoding = $rdcf->{"Encoding"};
	    chomp $def_encoding;
	    # print "Using $def_encoding as the default encoding\n";
	}
}

if($opt_chm) {
    $chmdir = "../chm";
    if(! -d $chmdir) {
	mkdir($chmdir, $dir_mod) or die "Could not create $chmdir: $!\n";
    }
}

if ($opt_latex) {
    $latex_d = file_path($dest, "latex");
    if(! -d $latex_d) {
	mkdir("$latex_d", $dir_mod) or die "Could not create $latex_d: $!\n";
    }
}
if ($opt_example) {
    $Rex_d = file_path($dest, "R-ex");
    if(! -d $Rex_d) {
	mkdir("$Rex_d", $dir_mod) or die "Could not create $Rex_d: $!\n";
    }
}

print " >>> Building/Updating help pages for package '$pkg'\n";
print "     Formats: ";
print "text " if $opt_txt;
print "html " if $opt_html;
print "latex " if $opt_latex;
print "example " if $opt_example;
print "chm " if $opt_chm;
print "\n";


if($opt_html || $opt_chm){
    %htmlindex = read_htmlindex($lib);
    if ($lib ne $mainlib) {
	# ensure mainlib is read
	%basehtmlindex = read_htmlindex($mainlib);
	foreach $topic (keys %htmlindex) {
	    $basehtmlindex{$topic} = $htmlindex{$topic};
	}
	%htmlindex = %basehtmlindex;
    }
    # give priority to standard packages
    foreach $pkg ("base", "utils", "graphics", "grDevices", "stats", 
		  "datasets", "methods") {
	my %pkghtmlindex = read_htmlpkgindex($mainlib, $pkg);
	foreach $topic (keys %pkghtmlindex) {
	    $basehtmlindex{$topic} = $pkghtmlindex{$topic};
	}
    }

    # make sure that references are resolved first to this package
    my %thishtmlindex = read_htmlpkgindex($lib, $pkg);
    foreach $topic (keys %thishtmlindex) {
	$htmlindex{$topic} = $thishtmlindex{$topic};
    }
}

format STDOUT =
  @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @<<<<<< @<<<<<< @<<<<<< @<<<<<< @<<<<<<
  $manfilebase, $textflag, $htmlflag, $latexflag, $exampleflag, $chmflag
.

foreach $manfile (@mandir) {
    ## Should only process files starting with [A-Za-z0-9] and with
    ## suffix .Rd or .rd, according to `Writing R Extensions'.
    if($manfile =~ /\.[Rr]d$/) {
	$manfilebase = basename($manfile, (".Rd", ".rd"));
	if(! ($manfilebase =~ /^[A-Za-z0-9]/) ) {next;}
	$manage = (-M $manfile);
	$manfiles{$manfilebase} = $manfile;

	$textflag = $htmlflag = $latexflag = $exampleflag = $chmflag = "";
	$types = "";
	undef $do_example;

	if($opt_txt){
	    my $targetfile = $manfilebase;
	    $destfile = file_path($dest, "help", $targetfile);
	    if(fileolder($destfile, $manage)) {
		$textflag = "text";
		$types .= "txt,";
	    }
	}

	if($opt_html){
	    my $targetfile = $manfilebase;
	    $misslink = "";
	    $destfile = file_path($dest, "html", $targetfile.".html");
	    if(fileolder($destfile, $manage)) {
		$htmlflag = "html";
		print "\t$destfile" if $opt_debug;
		$types .= "html,";
	    }
	}

	if($opt_latex){
	    my $targetfile = $manfilebase;
	    $destfile = file_path($dest, "latex", $targetfile.".tex");
	    if(fileolder($destfile, $manage)) {
		$latexflag = "latex";
		$types .= "latex,";
	    }
	}

	if($opt_example){
	    my $targetfile = $manfilebase;
	    $destfile = file_path($dest, "R-ex", $targetfile.".R");
	    if(fileolder($destfile, $manage)) {
		if(-f $destfile) {unlink $destfile;}
		$types .= "example,";
		$do_example = "yes";
	    }
	}

	Rdconv($manfile, $types, "", "$dest", $pkg, $version, 
	       $def_encoding) if $types ne "";
	if($do_example && -f $destfile) {$exampleflag = "example";}

	if($opt_chm){
	    my $targetfile = $manfilebase;
	    $misslink = "";
	    $destfile = "../chm/$targetfile.html";
	    if(fileolder($destfile,$manage)) {
		$chmflag = "chm";
		print "\t$destfile" if $opt_debug;
		Rdconv($manfile, "chm", "", "$destfile", $pkg, $version, 
		       $def_encoding);
	    }
	}

	write if ($textflag || $htmlflag || $latexflag || 
		  $exampleflag || $chmflag);
	print "     missing link(s): $misslink\n" 
	    if $htmlflag && length($misslink);
    }
}

# remove files not in source directory
if($opt_txt){
    my @destdir;
    opendir dest,  file_path($dest, "help");
    @destdir = sort(readdir(dest));
    closedir dest;
    foreach $destfile (@destdir) {
	if($destfile eq "." || $destfile eq ".." ||
	   $destfile eq "AnIndex") { next; }
	unlink file_path($dest, "help", $destfile)
	    unless defined $manfiles{$destfile};
    }
}
if($opt_html){
    my @destdir;
    opendir dest,  file_path($dest, "html");
    @destdir = sort(readdir(dest));
    closedir dest;
    foreach $destfile (@destdir) {
	$destfilebase = basename($destfile, ".html");
	if($destfile eq "." || $destfile eq ".." ||
	   $destfile eq "00Index.html") { next; }
	unlink file_path($dest, "html", $destfile)
	    unless defined $manfiles{$destfilebase};
    }
}
if($opt_latex){
    my @destdir;
    opendir dest,  file_path($dest, "latex");
    @destdir = sort(readdir(dest));
    closedir dest;
    foreach $destfile (@destdir) {
	$destfilebase = basename($destfile, ".tex");
	if($destfile eq "." || $destfile eq "..") { next; }
	unlink file_path($dest, "latex", $destfile)
	    unless defined $manfiles{$destfilebase};
    }
}
if($opt_example){
    my @destdir;
    opendir dest,  file_path($dest, "R-ex");
    @destdir = sort(readdir(dest));
    closedir dest;
    foreach $destfile (@destdir) {
	$destfilebase = basename($destfile, ".R");
	if($destfile eq "." || $destfile eq "..") { next; }
	unlink file_path($dest, "R-ex", $destfile)
	    unless defined $manfiles{$destfilebase};
    }
}
if($opt_chm){
    my @destdir;
    opendir dest,  "../chm";
    @destdir = sort(readdir(dest));
    closedir dest;
    foreach $destfile (@destdir) {
	$destfilebase = basename($destfile, (".html"));
	next unless $destfile =~ /\.html$/;
	next if $destfile eq "00Index.html";
	unlink "../chm/$destfile" unless defined $manfiles{$destfilebase};
    }
}

sub usage {
  print STDERR <<END;
Usage: R CMD $name [options] [pkg] [lib]

Install all help files for package pkg to library lib

Options:
  -h, --help		print short help message and exit
  -v, --version		print version info and exit
  -d, --debug           print debugging information
  -os, --OS             OS to assume: unix (default) or windows
  --rhome               R home directory, defaults to environment R_HOME
  --html                build HTML files    (default is all)
  --txt                 build text files    (default is all)
  --latex               build LaTeX files   (default is all)
  --example             build example files (default is all)
  --chm                 build CHM files     (only on Windows, default there)


Email bug reports to <r-bugs\@r-project.org>.
END
  exit 0;
}