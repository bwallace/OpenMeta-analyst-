# Spreadsheet::ParseExcel::SaveParser
#  by Kawai, Takanori (Hippo2000) 2001.5.1
# This Program is ALPHA version.
#//////////////////////////////////////////////////////////////////////////////
# Spreadsheet::ParseExcel:.SaveParser Objects
#//////////////////////////////////////////////////////////////////////////////
#==============================================================================
# Spreadsheet::ParseExcel::SaveParser::Workbook
#==============================================================================
package Spreadsheet::ParseExcel::SaveParser::Workbook;
use strict;
use warnings;

use base 'Spreadsheet::ParseExcel::Workbook';
our $VERSION = '0.06';

sub new {
    my($sPkg, $oBook) = @_;
    return undef unless(defined $oBook);
    my %oThis = %$oBook;
    bless \%oThis, $sPkg;
    
    # re-bless worksheets (and set their _Book properties !!!)
    my $sWkP = ref($sPkg) || "$sPkg";
    $sWkP =~ s/Workbook$/Worksheet/;
    map { bless($_, $sWkP); } @{$oThis{Worksheet}};
    map { $_->{_Book} = \%oThis; } @{$oThis{Worksheet}};
    return \%oThis;
}
#------------------------------------------------------------------------------
# Parse (for Spreadsheet::ParseExcel::SaveParser::Workbook)
#------------------------------------------------------------------------------
sub Parse {
    my($sClass, $sFile, $oWkFmt)=@_;
    my $oBook = Spreadsheet::ParseExcel::Workbook->Parse($sFile, $oWkFmt);
    bless $oBook, $sClass;

    # re-bless worksheets (and set their _Book properties !!!)
    my $sWkP = ref($sClass) || "$sClass";
    $sWkP =~ s/Workbook$/Worksheet/;
    map { bless($_, $sWkP); } @{$oBook->{Worksheet}};
    map { $_->{_Book} = $oBook; } @{$oBook->{Worksheet}};
    return $oBook;
}
#------------------------------------------------------------------------------
# SaveAs (for Spreadsheet::ParseExcel::SaveParser::Workbook)
#------------------------------------------------------------------------------
sub SaveAs {
    my ($oBook, $sName)=@_;
    # Create a new Excel workbook
    my $oWrEx = Spreadsheet::WriteExcel->new($sName);
    my %hFmt;

    my $iNo = 0;
    my @aAlH = ('left', 'left', 'center', 'right', 'fill', 'justify', 'merge', 'equal_space');
    my @aAlV = ('top' , 'vcenter', 'bottom', 'vjustify', 'vequal_space');

    foreach my $pFmt (@{$oBook->{Format}}) {
        my $oFmt = $oWrEx->addformat();    # Add Formats
        unless($pFmt->{Style}) {
            $hFmt{$iNo} = $oFmt;
            my $rFont = $pFmt->{Font};

            $oFmt->set_font($rFont->{Name});
            $oFmt->set_size($rFont->{Height});
            $oFmt->set_color($rFont->{Color});
            $oFmt->set_bold($rFont->{Bold});
            $oFmt->set_italic($rFont->{Italic});
            $oFmt->set_underline($rFont->{Underline});
            $oFmt->set_font_strikeout($rFont->{Strikeout});
            $oFmt->set_font_script($rFont->{Super});

            $oFmt->set_hidden($rFont->{Hidden});        #Add

            $oFmt->set_locked($pFmt->{Lock});

            $oFmt->set_align($aAlH[$pFmt->{AlignH}]);
            $oFmt->set_align($aAlV[$pFmt->{AlignV}]);
            if($pFmt->{Rotate}==0) {
                $oFmt->set_rotation(0);
            }
            elsif($pFmt->{Rotate}> 0) {  # Mainly ==90
                $oFmt->set_rotation(3);
            }
            elsif($pFmt->{Rotate} < 0) {  # Mainly == -90
                $oFmt->set_rotation(2);
            }
            $oFmt->set_num_format($oBook->{FmtClass}->FmtStringDef($pFmt->{FmtIdx}, $oBook));

            $oFmt->set_text_wrap($pFmt->{Wrap});

            $oFmt->set_pattern($pFmt->{Fill}->[0]);
            $oFmt->set_fg_color($pFmt->{Fill}->[1]) 
                        if(($pFmt->{Fill}->[1] >= 8) && ($pFmt->{Fill}->[1] <= 63));
            $oFmt->set_bg_color($pFmt->{Fill}->[2])
                        if(($pFmt->{Fill}->[2] >= 8) && ($pFmt->{Fill}->[2] <= 63));

            $oFmt->set_left  (($pFmt->{BdrStyle}->[0]>7)? 3: $pFmt->{BdrStyle}->[0]);
            $oFmt->set_right (($pFmt->{BdrStyle}->[1]>7)? 3: $pFmt->{BdrStyle}->[1]);
            $oFmt->set_top   (($pFmt->{BdrStyle}->[2]>7)? 3: $pFmt->{BdrStyle}->[2]);
            $oFmt->set_bottom(($pFmt->{BdrStyle}->[3]>7)? 3: $pFmt->{BdrStyle}->[3]);

            $oFmt->set_left_color  ($pFmt->{BdrColor}->[0])
                        if(($pFmt->{BdrColor}->[0] >= 8) && ($pFmt->{BdrColor}->[0] <= 63));
            $oFmt->set_right_color ($pFmt->{BdrColor}->[1])
                        if(($pFmt->{BdrColor}->[1] >= 8) && ($pFmt->{BdrColor}->[1] <= 63));
            $oFmt->set_top_color   ($pFmt->{BdrColor}->[2])
                        if(($pFmt->{BdrColor}->[2] >= 8) && ($pFmt->{BdrColor}->[2] <= 63));
            $oFmt->set_bottom_color($pFmt->{BdrColor}->[3])
                        if(($pFmt->{BdrColor}->[3] >= 8) && ($pFmt->{BdrColor}->[3] <= 63));
        }
        $iNo++;
    }
    for(my $iSheet=0; $iSheet < $oBook->{SheetCount} ; $iSheet++) {
        my $oWkS = $oBook->{Worksheet}[$iSheet];
        my $oWrS = $oWrEx->addworksheet($oWkS->{Name});
        #Landscape
        if(!$oWkS->{Landscape}) { # Landscape (0:Horizontal, 1:Vertical)
            $oWrS->set_landscape();
        }
        else {
            $oWrS->set_portrait();
        }
        #Protect
        if(defined $oWkS->{Protect}) { # Protect ('':NoPassword, Password:Password)
	    if ($oWkS->{Protect} ne '') {
		$oWrS->protect($oWkS->{Protect});
	    }
	    else {
		$oWrS->protect();
	    }
        }
        if(($oWkS->{FitWidth}==1) and ($oWkS->{FitHeight}==1)) {
            # Pages on fit with width and Heigt
            $oWrS->fit_to_pages($oWkS->{FitWidth}, $oWkS->{FitHeight});
            #Print Scale
            $oWrS->set_print_scale($oWkS->{Scale});
        }
        else {
            #Print Scale
            $oWrS->set_print_scale($oWkS->{Scale});
            # Pages on fit with width and Heigt
            $oWrS->fit_to_pages($oWkS->{FitWidth}, $oWkS->{FitHeight});
        }
        # Paper Size
        $oWrS->set_paper($oWkS->{PaperSize});
        # Margin
        $oWrS->set_margin_left($oWkS->{LeftMergin}     / 2.55);
        $oWrS->set_margin_right($oWkS->{RightMergin}   / 2.55);
        $oWrS->set_margin_top($oWkS->{TopMergin}       / 2.55);
        $oWrS->set_margin_bottom($oWkS->{BottomMergin} / 2.55);
        # HCenter
        $oWrS->center_horizontally() if($oWkS->{HCenter});
        # VCenter
        $oWrS->center_vertically() if($oWkS->{VCenter});
        # Header, Footer
        $oWrS->set_header($oWkS->{Header}, $oWkS->{HeaderMergin}/2.55);
        $oWrS->set_footer($oWkS->{Footer}, $oWkS->{FooterMergin}/2.55);
        # Print Area
        if(ref($oBook->{PrintArea}[$iSheet]) eq 'ARRAY') {
            my $raP;
            for $raP (@{$oBook->{PrintArea}[$iSheet]}) {
                $oWrS->print_area(@$raP);
            }
        }
            
        # Print Title
        my $raW;
        foreach $raW (@{$oBook->{PrintTitle}[$iSheet]->{Row}}) {
            $oWrS->repeat_rows(@$raW);
        }
        foreach $raW (@{$oBook->{PrintTitle}[$iSheet]->{Column}}) {
            $oWrS->repeat_columns(@$raW);
        }
        # Print Gridlines
        if($oWkS->{PrintGrid}==1) {
            $oWrS->hide_gridlines(0);
        }
        else {
            $oWrS->hide_gridlines(1);
        }
        # Print Headings
        if($oWkS->{PrintHeaders}) {
            $oWrS->print_row_col_headers();
        }
        # Horizontal Page Breaks
        $oWrS->set_h_pagebreaks(@{$oWkS->{HPageBreak}});
        # Veritical Page Breaks
        $oWrS->set_v_pagebreaks(@{$oWkS->{VPageBreak}});
=pod

            PageStart    => $oWkS->{PageStart},            # Page number for start
            UsePage      => $oWkS->{UsePage},              # Use own start page number
            NoColor      => $oWkS->{NoColor},               # Print in blcak-white
            Draft        => $oWkS->{Draft},                 # Print in draft mode
            Notes        => $oWkS->{Notes},                 # Print notes
            LeftToRight  => $oWkS->{LeftToRight},           # Left to Right
=cut
        for(my $iC = $oWkS->{MinCol} ;
                            defined $oWkS->{MaxCol} && $iC <= $oWkS->{MaxCol} ; $iC++) {            
            if(defined $oWkS->{ColWidth}[$iC]) {
                if($oWkS->{ColWidth}[$iC]>0) {
                    $oWrS->set_column($iC, $iC, $oWkS->{ColWidth}[$iC]);#, undef, 1) ;
                }
                else {
                    $oWrS->set_column($iC, $iC, 0, undef, 1) ;
                }
            }
        }
        for(my $iR = $oWkS->{MinRow} ; 
                defined $oWkS->{MaxRow} && $iR <= $oWkS->{MaxRow} ; $iR++) {
            $oWrS->set_row($iR, $oWkS->{RowHeight}[$iR]);
            for(my $iC = $oWkS->{MinCol} ;
                            defined $oWkS->{MaxCol} && $iC <= $oWkS->{MaxCol} ; $iC++) {

                my $oWkC = $oWkS->{Cells}[$iR][$iC];
                if($oWkC) {
                    if($oWkC->{Merged}) {
                        my $oFmtN = $oWrEx->addformat();
                        $oFmtN->copy($hFmt{$oWkC->{FormatNo}});
                        $oFmtN->set_merge(1);
                        $oWrS->write($iR , $iC, $oBook->{FmtClass}->TextFmt($oWkC->{Val}, $oWkC->{Code}), 
                            $oFmtN);
                    }
                    else {
                        $oWrS->write($iR , $iC, $oBook->{FmtClass}->TextFmt($oWkC->{Val}, $oWkC->{Code}), 
                            $hFmt{$oWkC->{FormatNo}});
                    }
                }
            }
        }
    }
    return $oWrEx
}
#------------------------------------------------------------------------------
# AddWorksheet (for Spreadsheet::ParseExcel::SaveParser::Workbook)
#------------------------------------------------------------------------------
sub AddWorksheet {
    my($oBook, $sName, %hAttr) = @_;
    $oBook->AddFormat if($#{$oBook->{Format}}<0);
    $hAttr{Name} ||= $sName;
    $hAttr{LeftMergin} ||= 0;
    $hAttr{RightMergin} ||= 0;
    $hAttr{TopMergin} ||= 0;
    $hAttr{BottomMergin} ||= 0;
    $hAttr{HeaderMergin} ||= 0;
    $hAttr{FooterMergin} ||= 0;
    $hAttr{FitWidth} ||= 0;
    $hAttr{FitHeight} ||= 0;
    $hAttr{PrintGrid} ||= 0;
    my $oWkS = Spreadsheet::ParseExcel::SaveParser::Worksheet->new(%hAttr);
    $oWkS->{_Book} = $oBook;
    $oWkS->{_SheetNo} = $oBook->{SheetCount};
    $oBook->{Worksheet}[$oBook->{SheetCount}] = $oWkS;
    $oBook->{SheetCount}++;
    return $oWkS; #$oBook->{SheetCount} - 1;
}
#------------------------------------------------------------------------------
# AddFont (for Spreadsheet::ParseExcel::SaveParser::Workbook)
#------------------------------------------------------------------------------
sub AddFont {
    my ($oBook, %hAttr) = @_;
    $hAttr{Name}     ||= 'Arial';
    $hAttr{Height}   ||= 10;
    $hAttr{Bold}     ||= 0;
    $hAttr{Italic}   ||= 0;
    $hAttr{Underline}||= 0;
    $hAttr{Strikeout}||= 0;
    $hAttr{Super}    ||= 0;
    push @{$oBook->{Font}}, 
        Spreadsheet::ParseExcel::Font->new(%hAttr);
    return $#{$oBook->{Font}};
}
#------------------------------------------------------------------------------
# AddFormat (for Spreadsheet::ParseExcel::SaveParser::Workbook)
#------------------------------------------------------------------------------
sub AddFormat {
    my ($oBook, %hAttr) = @_;
    $hAttr{Fill}     ||= [0, 0, 0];
    $hAttr{BdrStyle} ||= [0, 0, 0, 0];
    $hAttr{BdrColor} ||= [0, 0, 0, 0];
    $hAttr{AlignH} ||= 0;
    $hAttr{AlignV} ||= 0;
    $hAttr{Rotate} ||= 0;
    $hAttr{Landscape} ||= 0;
    $hAttr{FmtIdx} ||= 0;
    if(!defined($hAttr{Font})) {
        my $oFont;
        if(defined $hAttr{FontNo}) {
            $oFont = $oBook->{Font}[$hAttr{FontNo}];
        }
        elsif(!defined $oFont) {
            if($#{$oBook->{Font}}>=0) {
                $oFont = $oBook->{Font}[0];
            }
            else {
                my $iNo = $oBook->AddFont;
                $oFont = $oBook->{Font}[$iNo];
            }
        }
        $hAttr{Font} = $oFont;
    }
    push @{$oBook->{Format}}, 
        Spreadsheet::ParseExcel::Format->new(%hAttr);
    return $#{$oBook->{Format}};
}
#------------------------------------------------------------------------------
# AddCell (for Spreadsheet::ParseExcel::SaveParser::Workbook)
#------------------------------------------------------------------------------
sub AddCell {
    my($oBook, $iSheet, $iR, $iC, $sVal, $oCell, $sCode)=@_;
    my %rhKey;
    $oCell ||= 0;
    my $iFmt = (UNIVERSAL::isa($oCell, 'Spreadsheet::ParseExcel::Cell'))?
                $oCell->{FormatNo} : (ref($oCell))? 0: $oCell+0;
    $rhKey{FormatNo} = $iFmt;
    $rhKey{Format}   = $oBook->{Format}[$iFmt];
    $rhKey{Val}      = $sVal;
    $rhKey{Code}     = $sCode || '_native_';
    $oBook->{_CurSheet} = $iSheet;
    my $oNewCell = Spreadsheet::ParseExcel::_NewCell($oBook, $iR, $iC, %rhKey);
    Spreadsheet::ParseExcel::_SetDimension($oBook, $iR, $iC, $iC);
    return $oNewCell;
}
1;
#==============================================================================
# Spreadsheet::ParseExcel::SaveParser::Worksheet
#==============================================================================
package Spreadsheet::ParseExcel::SaveParser::Worksheet;
use strict;
use warnings;

use base 'Spreadsheet::ParseExcel::Worksheet';
our $VERSION = '0.06';


sub new {
  my ($sClass, %rhIni) = @_;
  $sClass->SUPER::new(%rhIni);  # returns object
}
#------------------------------------------------------------------------------
# AddCell (for Spreadsheet::ParseExcel::SaveParser::Worksheet)
#------------------------------------------------------------------------------
sub AddCell {
    my($oSelf, $iR, $iC, $sVal, $oCell, $sCode)=@_;
    $oSelf->{_Book}->AddCell($oSelf->{_SheetNo}, $iR, $iC, $sVal, $oCell, $sCode);
}
#------------------------------------------------------------------------------
# Protect (for Spreadsheet::ParseExcel::SaveParser::Worksheet)
#  - Password = undef   ->  No protect
#  - Password = ''      ->  Protected. No password
#  - Password = $pwd    ->  Protected. Password = $pwd
#------------------------------------------------------------------------------
sub Protect {
    my($oSelf, $sPassword)=@_;
    $oSelf->{Protect} = $sPassword;
}

#==============================================================================
# Spreadsheet::ParseExcel::SaveParser
#==============================================================================
package Spreadsheet::ParseExcel::SaveParser;
use strict;
use warnings;

use Spreadsheet::WriteExcel;
use base 'Spreadsheet::ParseExcel';
our $VERSION = '0.06';

use constant MagicCol => 1.14;
#------------------------------------------------------------------------------
# new (for Spreadsheet::ParseExcel::SaveParser)
#------------------------------------------------------------------------------
sub new {
    my($sPkg, %hKey) = @_;
    $sPkg->SUPER::new(%hKey);
}
#------------------------------------------------------------------------------
# Create
#------------------------------------------------------------------------------
sub Create {
    my($oThis, $oWkFmt)=@_;
#0. New $oBook
    my $oBook = Spreadsheet::ParseExcel::Workbook->new;
    $oBook->{SheetCount} = 0;
#2. Ready for format
    if ($oWkFmt) {
        $oBook->{FmtClass} = $oWkFmt;
    }
    else {
        $oBook->{FmtClass} = Spreadsheet::ParseExcel::FmtDefault->new;
    }
    return Spreadsheet::ParseExcel::SaveParser::Workbook->new($oBook);
}
#------------------------------------------------------------------------------
# Parse (for Spreadsheet::ParseExcel::SaveParser)
#------------------------------------------------------------------------------
sub Parse {
    my($oThis, $sFile, $oWkFmt)=@_;
    my $oBook = $oThis->SUPER::Parse($sFile, $oWkFmt);
    return undef unless(defined $oBook);
    return Spreadsheet::ParseExcel::SaveParser::Workbook->new($oBook);
}
#------------------------------------------------------------------------------
# SaveAs (for Spreadsheet::ParseExcel::SaveParser)
#------------------------------------------------------------------------------
sub SaveAs {
    my ($oThis, $oBook, $sName)=@_;
    $oBook->SaveAs($sName);
}
1;

__END__

=head1 NAME

Spreadsheet::ParseExcel::SaveParser - Expand of Spreadsheet::ParseExcel with Spreadsheet::WriteExcel

=head1 SYNOPSIS

    #1. Write an Excel file with previous data
    use strict;
    use Spreadsheet::ParseExcel::SaveParser;
    my $oExcel = new Spreadsheet::ParseExcel::SaveParser;
    my $oBook = $oExcel->Parse('temp.xls');
    #1.1.Update and Insert Cells
    my $iFmt = $oBook->{Worksheet}[0]->{Cells}[0][0]->{FormatNo};
    $oBook->AddCell(0, 0, 0, 'No(UPD)', 
        $oBook->{Worksheet}[0]->{Cells}[0][0]->{FormatNo});
    $oBook->AddCell(0, 1, 0, '304', $oBook->{Worksheet}[0]->{Cells}[0][0]);
    $oBook->AddCell(0, 1, 1, 'Kawai,Takanori', $iFmt);
    #1.2.add new worksheet
    my $iWkN = $oBook->AddWorksheet('Test');
    #1.3 Save
    $oExcel->SaveAs($oBook, 'temp.xls');  # as the same name
    $oExcel->SaveAs($oBook, 'temp1.xls'); # another name

    #2. Create new Excel file (most simple)
    use strict;
    use Spreadsheet::ParseExcel::SaveParser;
    my $oEx = new Spreadsheet::ParseExcel::SaveParser;
    my $oBook = $oEx->Create();
    $oBook->AddFormat;
    $oBook->AddWorksheet('NewWS');
    $oBook->AddCell(0, 0, 1, 'New Cell');
    $oEx->SaveAs($oBook, 'new.xls');

    #3. Create new Excel file(more complex)
    #!/usr/local/bin/perl
    use strict;
    use Spreadsheet::ParseExcel::SaveParser;
    my $oEx = new Spreadsheet::ParseExcel::SaveParser;
    my $oBook = $oEx->Create();
    my $iF1 = $oBook->AddFont(
            Name      => 'Arial',
            Height    => 11,
            Bold      => 1, #Bold
            Italic    => 1, #Italic
            Underline => 0,
            Strikeout => 0,
            Super     => 0,
        );
    my $iFmt =
    $oBook->AddFormat(
        Font => $oBook->{Font}[$iF1],
        Fill => [1, 10, 0],         # Filled with Red
                                    # cf. ParseExcel (@aColor)
        BdrStyle => [0, 1, 1, 0],   #Border Right, Top
        BdrColor => [0, 11, 0, 0],  # Right->Green
    );
    $oBook->AddWorksheet('NewWS');
    $oBook->AddCell(0, 0, 1, 'Cell', $iFmt);
    $oEx->SaveAs($oBook, 'new.xls');

I<new interface...>

    use strict;
    use Spreadsheet::ParseExcel::SaveParser;
    $oBook = 
        Spreadsheet::ParseExcel::SaveParser::Workbook->Parse('Excel/Test97.xls');
    my $oWs = $oBook->AddWorksheet('TEST1');
    $oWs->AddCell(10, 1, 'New Cell');
    $oBook->SaveAs('iftest.xls');

=head1 DESCRIPTION

Spreadsheet::ParseExcel::SaveParser : Expand of Spreadsheet::ParseExcel with Spreadsheet::WriteExcel

=head2 Functions

=over 4

=item new

I<$oExcel> = new Spreadsheet::ParseExcel::SaveParser();

Constructor.

=item Parse

I<$oWorkbook> = $oParse->Parse(I<$sFileName> [, I<$oFmt>]);

return L<"Workbook"> object.
if error occurs, returns undef.

=over 4

=item I<$sFileName>

name of the file to parse (Same as Spreadsheet::ParseExcel)

From 0.12 (with OLE::Storage_Lite v.0.06), 
scalar reference of file contents (ex. \$sBuff) or 
IO::Handle object (inclucdng IO::File etc.) are also available.

=item I<$oFmt>

Formatter Class to format the value of cells.

=back

=item Create

I<$oWorkbook> = $oParse->Create([I<$oFmt>]);

return new L<"Workbook"> object.
if error occurs, returns undef.

=over 4

=item I<$oFmt>

Formatter Class to format the value of cells.

=back

=item SaveAs

I<$oWorkbook> = $oParse->SaveAs( $oBook, $sName);

save $oBook image as an Excel file named $sName.

=over 4

=item I<$oBook>

An Excel Workbook object to save.

=back

=item I<$sName>

Name of new Excel file.

=back

=head2 Workbook

I<Spreadsheet::ParseExcel::SaveParser::Workbook>

Workbook is a subclass of Spreadsheet::ParseExcel::Workbook.
And has these methods :

=over 4

=item AddWorksheet

I<$oWorksheet> = $oBook->AddWorksheet($sName, %hProperty);

Create new Worksheet(Spreadsheet::ParseExcel::Worksheet).

=over 4

=item I<$sName>

Name of new Worksheet

=item I<$hProperty>

Property of new Worksheet.

=back

=item AddFont

I<$oWorksheet> = $oBook->AddFont(%hProperty);

Create new Font(Spreadsheet::ParseExcel::Font).

=over 4

=item I<$hProperty>

Property of new Worksheet.

=back

=item AddFormat

I<$oWorksheet> = $oBook->AddFormat(%hProperty);

Create new Format(Spreadsheet::ParseExcel::Format).

=over 4

=item I<$hProperty>

Property of new Format.

=back

=item AddCell

I<$oWorksheet> = $oBook->AddCell($iWorksheet, $iRow, $iCol, $sVal, $iFormat [, $sCode]);

Create new Cell(Spreadsheet::ParseExcel::Cell).

=over 4

=item I<$iWorksheet>

Number of Worksheet

=back

=over 4

=item I<$iRow>

Number of row

=back

=over 4

=item I<$sVal>

Value of the cell.

=back

=over 4

=item I<$iFormat>

Number of format for use. To specify just same as another cell,
you can set it like below:

ex.

  $oCell=$oWorksheet->{Cells}[0][0]; #Just a sample
  $oBook->AddCell(0, 1, 0, 'New One', $oCell->{FormatNo});
    #or 
  $oBook->AddCell(0, 1, 0, 'New One', $oCell);

=back

=over 4

=item I<$sCode>

  Character code

=back

=back

=head2 Worksheet

I<Spreadsheet::ParseExcel::SaveParser::Worksheet>

Worksheet is a subclass of Spreadsheet::ParseExcel::Worksheet.
And has these methods :

=over 4

=item AddCell

I<$oWorksheet> = $oWkSheet->AddCell($iRow, $iCol, $sVal, $iFormat [, $sCode]);

Create new Cell(Spreadsheet::ParseExcel::Cell).

=over 4

=item I<$iRow>

Number of row

=back

=over 4

=item I<$sVal>

Value of the cell.

=back

=over 4

=item I<$iFormat>

Number of format for use. To specify just same as another cell,
you can set it like below:

ex.

  $oCell=$oWorksheet->{Cells}[0][0]; #Just a sample
  $oWorksheet->AddCell(1, 0, 'New One', $oCell->{FormatNo});
    #or 
  $oWorksheet->AddCell(1, 0, 'New One', $oCell);

=back

=over 4

=item I<$sCode>

  Character code

=back

=back

=head1 MORE INFORMATION

Please visit my Wiki page.
 I'll add sample at :
    http://www.hippo2000.info/cgi-bin/KbWikiE/KbWiki.pl

=head1 Known Problem

-Only last print area will remain. (Others will be removed)

=head1 AUTHOR

Kawai Takanori (Hippo2000) kwitknr@cpan.org

    http://member.nifty.ne.jp/hippo2000/            (Japanese)
    http://member.nifty.ne.jp/hippo2000/index_e.htm (English)

=head1 SEE ALSO

XLHTML, OLE::Storage, Spreadsheet::WriteExcel, OLE::Storage_Lite

This module is based on herbert within OLE::Storage and XLHTML.

=head1 COPYRIGHT

Copyright (c) 2000-2002 Kawai Takanori and Nippon-RAD Co. OP Division
All rights reserved.

You may distribute under the terms of either the GNU General Public
License or the Artistic License, as specified in the Perl README file.

=head1 ACKNOWLEDGEMENTS

First of all, I would like to acknowledge valuable program and modules :
XHTML, OLE::Storage and Spreadsheet::WriteExcel.

In no particular order: Yamaji Haruna, Simamoto Takesi, Noguchi Harumi, 
Ikezawa Kazuhiro, Suwazono Shugo, Hirofumi Morisada, Michael Edwards, Kim Namusk 
and many many people + Kawai Mikako.

=cut
