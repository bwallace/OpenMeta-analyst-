# Spreadsheet::ParseExcel::FmtJapan2
#  by Kawai, Takanori (Hippo2000) 2001.2.2
# This Program is ALPHA version.
#==============================================================================
package Spreadsheet::ParseExcel::FmtJapan2;
use strict;
use warnings;

use Jcode;
use Unicode::Map;
use base 'Spreadsheet::ParseExcel::FmtJapan';
our $VERSION = '0.05'; # 

#------------------------------------------------------------------------------
# new (for Spreadsheet::ParseExcel::FmtJapan2)
#------------------------------------------------------------------------------
sub new {
    my($sPkg, %hKey) = @_;
    my $oMap = Unicode::Map->new('CP932Excel');
    die "NO MAP FILE CP932Excel!!" 
        unless(-r Unicode::Map->mapping("CP932Excel"));

    my $oThis={ 
        Code => $hKey{Code},
        _UniMap => $oMap,
    };
    bless $oThis;
    $oThis->SUPER::new(%hKey);
    return $oThis;
}
#------------------------------------------------------------------------------
# TextFmt (for Spreadsheet::ParseExcel::FmtJapan2)
#------------------------------------------------------------------------------
sub TextFmt {
    my($oThis, $sTxt, $sCode) =@_;
#    $sCode = 'sjis' if((! defined($sCode)) || ($sCode eq '_native_'));
    if($oThis->{Code}) {
        if(! defined($sCode)) {
            $sTxt =~ s/(.)/\x00$1/sg;
            $sTxt = $oThis->{_UniMap}->from_unicode($sTxt);
        }
        elsif($sCode eq 'ucs2') {
            $sTxt = $oThis->{_UniMap}->from_unicode($sTxt);
        }
        return Jcode::convert($sTxt, $oThis->{Code}, 'sjis');
    }
    else {
        return $sTxt;
    }
}
1;
