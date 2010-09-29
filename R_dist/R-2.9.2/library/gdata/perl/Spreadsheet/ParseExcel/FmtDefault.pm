# Spreadsheet::ParseExcel::FmtDefault
#  by Kawai, Takanori (Hippo2000) 2001.2.2
# This Program is ALPHA version.
#==============================================================================
package Spreadsheet::ParseExcel::FmtDefault;
use strict;
use warnings;

use Spreadsheet::ParseExcel::Utility qw(ExcelFmt);
our $VERSION = '0.05'; # 

my %hFmtDefault = (
    0x00 => '@',
    0x01 => '0',
    0x02 => '0.00',
    0x03 => '#,##0',
    0x04 => '#,##0.00',
    0x05 => '($#,##0_);($#,##0)',
    0x06 => '($#,##0_);[RED]($#,##0)',
    0x07 => '($#,##0.00_);($#,##0.00_)',
    0x08 => '($#,##0.00_);[RED]($#,##0.00_)',
    0x09 => '0%',
    0x0A => '0.00%',
    0x0B => '0.00E+00',
    0x0C => '# ?/?',
    0x0D => '# ??/??',
    0x0E => 'm-d-yy',
    0x0F => 'd-mmm-yy',
    0x10 => 'd-mmm',
    0x11 => 'mmm-yy',
    0x12 => 'h:mm AM/PM',
    0x13 => 'h:mm:ss AM/PM',
    0x14 => 'h:mm',
    0x15 => 'h:mm:ss',
    0x16 => 'm-d-yy h:mm',
#0x17-0x24 -- Differs in Natinal
    0x25 => '(#,##0_);(#,##0)',
    0x26 => '(#,##0_);[RED](#,##0)',
    0x27 => '(#,##0.00);(#,##0.00)',
    0x28 => '(#,##0.00);[RED](#,##0.00)',
    0x29 => '_(*#,##0_);_(*(#,##0);_(*"-"_);_(@_)',
    0x2A => '_($*#,##0_);_($*(#,##0);_(*"-"_);_(@_)',
    0x2B => '_(*#,##0.00_);_(*(#,##0.00);_(*"-"??_);_(@_)',
    0x2C => '_($*#,##0.00_);_($*(#,##0.00);_(*"-"??_);_(@_)',
    0x2D => 'mm:ss',
    0x2E => '[h]:mm:ss',
    0x2F => 'mm:ss.0',
    0x30 => '##0.0E+0',
    0x31 => '@',
);
#------------------------------------------------------------------------------
# new (for Spreadsheet::ParseExcel::FmtDefault)
#------------------------------------------------------------------------------
sub new {
    my($sPkg, %hKey) = @_;
    my $oThis={ 
    };
    bless $oThis;
    return $oThis;
}
#------------------------------------------------------------------------------
# TextFmt (for Spreadsheet::ParseExcel::FmtDefault)
#------------------------------------------------------------------------------
sub TextFmt {
    my($oThis, $sTxt, $sCode) =@_;
    return $sTxt if((! defined($sCode)) || ($sCode eq '_native_'));
    return pack('U*', unpack('n*', $sTxt));
}
#------------------------------------------------------------------------------
# FmtStringDef (for Spreadsheet::ParseExcel::FmtDefault)
#------------------------------------------------------------------------------
sub FmtStringDef {
    my($oThis, $iFmtIdx, $oBook, $rhFmt) =@_;
    my $sFmtStr = $oBook->{FormatStr}->{$iFmtIdx};

    if(!(defined($sFmtStr)) && defined($rhFmt)) {
        $sFmtStr = $rhFmt->{$iFmtIdx};
    }
    $sFmtStr = $hFmtDefault{$iFmtIdx} unless($sFmtStr);
    return $sFmtStr;
}
#------------------------------------------------------------------------------
# FmtString (for Spreadsheet::ParseExcel::FmtDefault)
#------------------------------------------------------------------------------
sub FmtString {
    my($oThis, $oCell, $oBook) =@_;

    my $sFmtStr = $oThis->FmtStringDef(
        $oBook->{Format}[$oCell->{FormatNo}]->{FmtIdx}, $oBook);

    unless(defined($sFmtStr)) {
        if ($oCell->{Type} eq 'Numeric') {
            if(int($oCell->{Val}) != $oCell->{Val}) {
                $sFmtStr = '0.00';
            }
            else {
                $sFmtStr = '0';
            }
        }
        elsif($oCell->{Type} eq 'Date') {
            if(int($oCell->{Val}) <= 0) {
                $sFmtStr = 'h:mm:ss';
            }
            else {
                $sFmtStr = 'm-d-yy';
            }
        }
        else {
            $sFmtStr = '@';
        }
    }
    return $sFmtStr;
}
#------------------------------------------------------------------------------
# ValFmt (for Spreadsheet::ParseExcel::FmtDefault)
#------------------------------------------------------------------------------
sub ValFmt {
    my($oThis, $oCell, $oBook) =@_;

    my($Dt, $iFmtIdx, $iNumeric, $Flg1904);

    if ($oCell->{Type} eq 'Text') {
        $Dt = ((defined $oCell->{Val}) && ($oCell->{Val} ne ''))? 
            $oThis->TextFmt($oCell->{Val}, $oCell->{Code}):''; 
    }
    else {      
        $Dt = $oCell->{Val};
    }
    $Flg1904  = $oBook->{Flg1904};
    my $sFmtStr = $oThis->FmtString($oCell, $oBook);
    return ExcelFmt($sFmtStr, $Dt, $Flg1904, $oCell->{Type});
}
#------------------------------------------------------------------------------
# ChkType (for Spreadsheet::ParseExcel::FmtDefault)
#------------------------------------------------------------------------------
sub ChkType {
    my($oPkg, $iNumeric, $iFmtIdx) =@_;
    if ($iNumeric) {
        if((($iFmtIdx >= 0x0E) && ($iFmtIdx <= 0x16)) ||
           (($iFmtIdx >= 0x2D) && ($iFmtIdx <= 0x2F))) {
            return "Date";
        }
        else {
            return "Numeric";
        }
    }
    else {
        return "Text";
    }
}
1;
