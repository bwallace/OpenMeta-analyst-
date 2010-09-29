# Spreadsheet::ParseExcel::FmtJapan
#  by Kawai, Takanori (Hippo2000) 2001.2.2
# This Program is ALPHA version.
#==============================================================================
package Spreadsheet::ParseExcel::FmtJapan;
use strict;
use warnings;

use Jcode;
use base 'Spreadsheet::ParseExcel::FmtDefault';
our $VERSION = '0.05';

my %hFmtJapan = (
    0x00 => '@',
    0x01 => '0',
    0x02 => '0.00',
    0x03 => '#,##0',
    0x04 => '#,##0.00',
    0x05 => '(\\#,##0_);(\\#,##0)',
    0x06 => '(\\#,##0_);[RED](\\#,##0)',
    0x07 => '(\\#,##0.00_);(\\#,##0.00_)',
    0x08 => '(\\#,##0.00_);[RED](\\#,##0.00_)',
    0x09 => '0%',
    0x0A => '0.00%',
    0x0B => '0.00E+00',
    0x0C => '# ?/?',
    0x0D => '# ??/??',
#    0x0E => 'm/d/yy',
    0x0E => 'yyyy/m/d',
    0x0F => 'd-mmm-yy',
    0x10 => 'd-mmm',
    0x11 => 'mmm-yy',
    0x12 => 'h:mm AM/PM',
    0x13 => 'h:mm:ss AM/PM',
    0x14 => 'h:mm',
    0x15 => 'h:mm:ss',
#    0x16 => 'm/d/yy h:mm',
    0x16 => 'yyyy/m/d h:mm',

#0x17-0x24 -- Differs in Natinal
    0x1E => 'm/d/yy',
    0x1F => 'yyyy"年"m"月"d"日"',
    0x20 => 'h"時"mm"分"',
    0x21 => 'h"時"mm"分"ss"秒"',
#0x17-0x24 -- Differs in Natinal
    0x25 => '(#,##0_);(#,##0)',
    0x26 => '(#,##0_);[RED](#,##0)',
    0x27 => '(#,##0.00);(#,##0.00)',
    0x28 => '(#,##0.00);[RED](#,##0.00)',
    0x29 => '_(*#,##0_);_(*(#,##0);_(*"-"_);_(@_)',
    0x2A => '_(\\*#,##0_);_(\\*(#,##0);_(*"-"_);_(@_)',
    0x2B => '_(*#,##0.00_);_(*(#,##0.00);_(*"-"??_);_(@_)',
    0x2C => '_(\\*#,##0.00_);_(\\*(#,##0.00);_(*"-"??_);_(@_)',
    0x2D => 'mm:ss',
    0x2E => '[h]:mm:ss',
    0x2F => 'mm:ss.0',
    0x30 => '##0.0E+0',
    0x31 => '@',

    0x37 => 'yyyy"年"m"月"',        
    0x38 => 'm"月"d"日"',       
    0x39 => 'ge.m.d',
    0x3A => 'ggge"年"m"月"d"日"',
);
my $_Code;
#------------------------------------------------------------------------------
# new (for Spreadsheet::ParseExcel::FmtJapan)
#------------------------------------------------------------------------------
sub new {
    my($sPkg, %hKey) = @_;
    my $oThis={ 
        Code => $hKey{Code},
    };
    if($oThis->{Code}) {
        foreach my $sKey (keys %hFmtJapan) {
            $hFmtJapan{$sKey} = 
                Jcode::convert($hFmtJapan{$sKey}, $oThis->{Code}, 'euc');
        }
        $_Code = $oThis->{Code};
    }
    bless $oThis;
    return $oThis;
}
#------------------------------------------------------------------------------
# TextFmt (for Spreadsheet::ParseExcel::FmtJapan)
#------------------------------------------------------------------------------
sub TextFmt {
    my($oThis, $sTxt, $sCode) =@_;

    if($oThis->{Code}) {
        if(! defined($sCode)) {
            $sTxt =~ s/(.)/\x00$1/sg;
            $sCode = 'ucs2';
        }
        elsif($sCode eq '_native_') {
            $sCode = 'sjis';
        }
        return Jcode::convert($sTxt, $oThis->{Code}, $sCode);
    }
    else {
        return $sTxt;
    }
}
#------------------------------------------------------------------------------
# FmtStringDef (for Spreadsheet::ParseExcel::FmtJapan)
#------------------------------------------------------------------------------
sub FmtStringDef {
    my($oThis, $iFmtIdx, $oBook) =@_;
    return $oThis->SUPER::FmtStringDef($iFmtIdx, $oBook, \%hFmtJapan);
}
#------------------------------------------------------------------------------
# ValFmt (for Spreadsheet::ParseExcel::FmtJapan)
#------------------------------------------------------------------------------
sub ValFmt {
    my($oThis, $oCell, $oBook) =@_;
    return $oThis->SUPER::ValFmt($oCell, $oBook);
}
#------------------------------------------------------------------------------
# ChkType (for Spreadsheet::ParseExcel::FmtJapan)
#------------------------------------------------------------------------------
sub ChkType {
    my($oPkg, $iNumeric, $iFmtIdx) =@_;
# Is there something special for Japan?
    return $oPkg->SUPER::ChkType($iNumeric, $iFmtIdx);
}
#------------------------------------------------------------------------------
# CnvNengo (for Spreadsheet::ParseExcel::FmtJapan)
#------------------------------------------------------------------------------
sub CnvNengo {
    my($iKind, @aTime) = @_;
    my $iWk = sprintf('%04d%02d%02d', $aTime[5], $aTime[4], $aTime[3]);
    if($iWk    lt '19120730') {
        my $iY = $aTime[5] - 1867;
        return ($iKind == 1)? "M$iY" : 
                Jcode::convert("明治$iY", $_Code, 'euc');
    }
    elsif($iWk lt '19261225') {
        my $iY = $aTime[5] - 1911;
        return ($iKind == 1)? "T$iY" : 
                Jcode::convert("大正$iY", $_Code, 'euc');
    }
    elsif($iWk lt '19890108' ) {
        my $iY = $aTime[5] - 1925;
        return ($iKind == 1)? "S$iY" : 
                Jcode::convert("昭和$iY", $_Code, 'euc');
    }
    else {
        my $iY = $aTime[5] - 1988;
        return ($iKind == 1)? "H$iY" :
                Jcode::convert("平成$iY", $_Code, 'euc');
    }
}

1;
