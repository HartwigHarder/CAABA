// en FPC v>=2.0.0

// = imdouble.pas ====================================================
// (isotopic) mechanism doubling tool
//
// im-double main source module
// requires: all is done through imcom.inc
//
// [S. Gromov, MPIC, 2007-2009]
// ===================================================================

program imdouble;

// - conditional defines -------------------------------------------

{$DEFINE DBL}             // for imcom: compiling imdouble

{$DEFINE noDBL_EXPL}        // explicit doubling - original reactions are replaced
                          // can be defined during compilation with -dDBL_EXPL switch

{$DEFINE xUSE_PT}          // using passive tracers, may be OFF

{$DEFINE xUSE_PT_KIE}      // defines whether to add PTs to all
                          // KIE-reactions (for monitoring)

{$DEFINE USE_KRSIND}      // defines whether to output the list of
                          // KIE-related specs indices (for correction)

{$DEFINE noPRECISE_FRACISO_IEX}  // defines whether to treat IEX in case of "fractional" isotopologue
                                 // (i.e. 1 class) very carefully (i.e. weighting of reaction rate to regular-minor)

{$DEFINE USE_DKRATE}      // defines whether to put reaction rates in the eqn header as a
                          // separate variables and use them in multiple doubled reactions

{$DEFINE IEX_ONWARD_RATES}  // defines whether isotope exchange rates are given as onward for the
                            // first reactant, or referred to the regular reaction (affects the rate coefficient)

{$DEFINE noIEX_REGREF}    // defines whether to enable created regular reactions as a reference
                          // for the isotope exchange reactions

// -----------------------------------------------------------------

//  linking common utils/types include file for im-double/tag
{$I imcom.inc}

// -----------------------------------------------------------------

// - DOUBLING PART -------------------------------------------------

{$IFDEF USE_DKRATE}
const prev_ptracs_intr : ansistring = ''; // previous value of ptracs_intr
      dkratesyntax = 'dbl_k@';            // naming of vars for duped r.rate coeffs
{$ENDIF}

const dsrcsubsyntax = 'dbl_src_f@';       // naming of vars for source substit. fracs

const stoiformat = '0.0######';

// main doubling routine
procedure doubleit(fname : string);

function giveiso(spec : string; no : integer) : string;
var n : integer;
begin

n:=no_tsl(spec);      // get a number of a species in tsl
if (n=0) or (no<1) or (no>_isos) then
   giveiso:=spec
else
    giveiso:=tsl[n].isos[no];

end;

var f : textfile;     // output file

// creation of inline-part for updating of a
// rate constants coefficients for a specified sources
procedure addinline4subs;

var i, j, k : word;
    a : ansistring;
    srcspecs : array[1..max_tsl] of nstr;
   _srcspecs : integer; 
    gotcha : boolean;
begin

// making the list of all "source" species used in source specification
_srcspecs:=0;
fillchar(srcspecs,sizeof(srcspecs),0);
for i:=1 to _eqs do
    if ((eqs[i].itag) and (eqs[i].isrc)) then
    for j:=1 to src[eqs[i].nsrc]._trans do
        begin
        gotcha:=false;              // checking if a spec in the list already
        for k:=1 to _srcspecs do
            if (src[eqs[i].nsrc].trans[j].src=srcspecs[k]) then gotcha:=true;
        if (not(gotcha) and is_usedspec(src[eqs[i].nsrc].trans[j].src)) and
           ((src[eqs[i].nsrc].trans[j].src<>eqs[i].educ[1]) and   // if the source is on the lef side of eq, no fraction calc is needed
            (src[eqs[i].nsrc].trans[j].src<>eqs[i].educ[2])) then
           begin
           inc(_srcspecs);
           srcspecs[_srcspecs]:=src[eqs[i].nsrc].trans[j].src;
           end;
        end;

{$IFDEF PRECISE_FRACISO_IEX}
// source species for IEX for "fractional" isotopologues
if not(_isos>1) then
   for i:=1 to _eqs do
       if ((eqs[i].itag) and (eqs[i].iiex)) then
         with iex[eqs[i].niex] do
          for j:=1 to 2 do
              begin
              gotcha:=false;              // checking if a spec in the list already
              for k:=1 to _srcspecs do
                  if (tsl[exspec[j]].spec=srcspecs[k]) then gotcha:=true;
              if (not(gotcha) and is_usedspec(tsl[exspec[j]].spec)) then
                 begin
                 inc(_srcspecs);
                 srcspecs[_srcspecs]:=tsl[exspec[j]].spec;
                 end;
              end;
{$ENDIF}

// in case we've nothing to add
if (_srcspecs=0) then exit;

// GLOBAL part
writeln(f,'#INLINE F90_GLOBAL');
writeln(f,'! ---------------- [',tagname,'] - inline code ----------------');
writeln(f,'! reaction rates modifiers according to the specified source isotopic composition');

writeln(f,'  REAL(dp) :: dbl_src_temp           ! variable used for fractions calculation');

for i:=1 to _srcspecs do
    begin
//       writeln(f,'! '+src[i].spec+' isotopologue fractions');
    a:='  REAL(dp) :: ';
    for j:=1 to _isos do
        a:=a+substr(dsrcsubsyntax,'@',clsname[j]+srcspecs[i])+', ';
    setlength(a,length(a)-2);
    writeln(f,wrap(a,2,4));
    end;
writeln(f,'#ENDINLINE');

// RCONST part
writeln(f,'#INLINE F90_RCONST');
writeln(f,'! ---------------- [',tagname,'] - inline code ----------------');
writeln(f,'! calculation of the isotopologue fractions');
//writeln(f,'!');

for i:=1 to _srcspecs do
    begin
    // total composition
    a:='(';
    if (_isos>1) then
       for j:=1 to _isos do
           a+='C('+substr(sisyntax,'@',clsname[j]+srcspecs[i])+')+'            // substr(sisyntax,'@',clsname[j]+srcspecs[i]) should be replaced with isos[]
    else
        a+='C(ind_'+srcspecs[i]+')+';
    setlength(a,length(a)-1);
    a+=')';
    if not(_isos>1) then
       a+='       ! <!> warn: 1 class used, thus weighting to regular ';
    writeln(f,'  dbl_src_temp = ',a);
    writeln(f,'  IF (dbl_src_temp .GT. 0.0_dp) THEN');
    for j:=1 to _isos do
        writeln(f,'    '+substr(dsrcsubsyntax,'@',clsname[j]+srcspecs[i])+' = ',
                    'C(ind_'+clsname[j]+srcspecs[i]+') / dbl_src_temp');
    writeln(f,'  ELSE');
    for j:=1 to _isos do
        writeln(f,'    '+substr(dsrcsubsyntax,'@',clsname[j]+srcspecs[i])+' = 0.0_dp');
    writeln(f,'  ENDIF');
//  writeln(f,'!');
    end;
writeln(f,'#ENDINLINE');

end;

// creation of inline-part for updating of a
// rate constants coefficients for a doubled equations
procedure addinline4dblrates;
var i : word;
    a : ansistring;
    any2rate : boolean;
    
begin
{$IFDEF USE_DKRATE}
// detecting if there are reactions not yet processed
any2rate:=false;
for i:=1 to _eqs do
    if (eqs[i].itag) and (eqs[i].etag<2) then
       any2rate:=true;
if not(any2rate) then exit;

// GLOBAL part
writeln(f,'#INLINE F90_GLOBAL');
writeln(f,'! ---------------- [',tagname,'] - inline code ----------------');
writeln(f,'!');
writeln(f,'! reaction rates variables (to be used in multiplied doubled reactions)');
a:='  REAL(dp) :: ';
for i:=1 to _eqs do
    if (eqs[i].itag) and (eqs[i].etag<2) then
//       if (pos(':'+eqs[i].abbr+'>',prev_ptracs_intr)=0) then    // var. might have been added in the previous conf.
          a+=substr(dkratesyntax,'@',eqs[i].abbr)+', ';
setlength(a,length(a)-2);
writeln(f,wrap(a,2,4));
writeln(f,'#ENDINLINE');

// RCONST part
writeln(f,'#INLINE F90_RCONST');
writeln(f,'! ---------------- [',tagname,'] - inline code ----------------');
writeln(f,'! reaction rates variables calculation (to be calculated once per rates evaluation)');
writeln(f,'!');
for i:=1 to _eqs do
    if (eqs[i].itag) and (eqs[i].etag<2) then
//       if (pos(':'+eqs[i].abbr+'>',prev_ptracs_intr)=0) then    // var. might have been added in the previous conf.
          writeln(f,'  '+substr(dkratesyntax,'@',eqs[i].abbr)+' = '+trim(imcom_ext4marks(eqs[i].phys,'}',';')+';'));
writeln(f,'#ENDINLINE');
{$ENDIF}	  

{$IFDEF USE_PT}
prev_ptracs_intr:=ptracs_intr;
{$ENDIF}
end;

// - doubleit body

var i, j, k, l, e, d : integer;
    a, b : ansistring;
    qae : array[1..2] of integer;   // quantity of atoms in educts
    qap : integer;                  //  -- " --             product
    af, pf, savstoi : real;
{$IFDEF DBL_EXPL}
    x : integer;
{$ENDIF}

// main part

begin

write('doubleit(',paramstr(1),',',paramstr(2),',',fname,'): ');

write('/',{$IFNDEF DBL_EXPL}'NOT ',{$ENDIF}'EXPLICIT/ ');

assign(f,fname);
rewrite(f);

// some info
// writeln(f,eqnfile[1].line);
// writeln(f);
writeln(f,'#INLINE F90_GLOBAL');
writeln(f,'! -------------------------------------------------------------------------');
writeln(f,'! current mechanism (',paramstr(1),') is isotopically doubled by [imdouble]');
writeln(f,'! configuration: ',tagname);
  write(f,'! atom: ',isoatom,', ',_isos,' isotopologues of masses ');
for i:=1 to _isos do
    write(f,clsname[i],' ');
writeln(f);
writeln(f,'! # of doubled / added species: ',_utsl,' / ',(_utsl+1)*_isos+1);
writeln(f,'! # of reactions in the selected mechanism: ',_eqs);
writeln(f,'! # of tagged reactions: ',nooftagreac,' (',_src,' subs)');
{$IFDEF USE_PT}
writeln(f,'! # of added passive tracers: ',_ptracs_conf,' (',_ptracs_conf-nooftagreac,' add.kie)');
{$ENDIF}
writeln(f,'! current doubling is ',{$IFNDEF DBL_EXPL}'NOT ',{$ENDIF}'EXPLICIT');
writeln(f,'! PTs added: ',{$IFDEF USE_PT}'YES'{$ELSE}'NO'{$ENDIF},
             ', for KIE monitoring: ',{$IFDEF USE_PT_KIE}'YES'{$ELSE}'NO'{$ENDIF},'');
writeln(f,'! =', datetimetostr(now),'=');
writeln(f,'! --v-- further comes modified mechanism ----------------------------------');
writeln(f,'#ENDINLINE');
writeln(f);

for l:=1 to _eqnfile do
    if (eqnfile[l].iseq) then
       begin

       if (eqs[eqnfile[l].eqno].itag) then           // working with equation or just passing line
          with eqs[eqnfile[l].eqno] do

          begin

{$IFNDEF DBL_EXPL}
          // disabling creation of reference reaction for isotope exchange
 {$IFNDEF IEX_REGREF}
          if (iiex) then write(f,'// ');
 {$ENDIF}
          // first, original eq with a production tracer added
          write(f,'<',abbr,'> ');

          // educts
          write(f,educ[1]);
          if (educ[2]<>'') then write(f,' + ',educ[2]);
          write(f,' = ');

          // products
          for j:=1 to _prod do
              begin
              if (j>1) then
                 if (stoi[j]>0) then write(f,' + ');    // avoiding "-" products
              if (stoi[j]<>1.0) then
                 write(f,floattostr(stoi[j]),' ');
              write(f,prod[j]);
              end;
 {$IFDEF USE_PT}
          if (pos(substr(ptsyntax,'@',abbr),eqnfile[l].line)=0) then
             // adding production tracer
             write(f,' + '+substr(ptsyntax,'@',abbr));
 {$ENDIF}
          // adding reaction rate
 {$IFNDEF USE_DKRATE}
          write(f,' : '+trim(phys));
 {$ELSE}
          write(f,' :'+' {%*} '+substr(dkratesyntax,'@',abbr)+'; {&&}');
 {$ENDIF}

{$ENDIF}
          // writing "ever tagged" number
          write(f,' {'+_etagspsy,etag:1,'}');
          writeln(f);
          
          // now the doubled reactions come

          // ok, is it a isotope exchange/subsitution or normal equation for tagging?
          if (not(isrc) and not(iiex)) then
             begin

//{$ELSE}
//`             // if there is only one tagged educt and it is #2 then swapping educts in explicit doubling
//`             if (in_tsl(educ[2]) and not(in_tsl(educ[1]))) then
//`                begin a:=educ[1]; educ[1]:=educ[2]; educ[2]:=a; end;
//{$ENDIF}

             // q-ty of atoms in educts
             for e:=1 to 2 do
                 if (in_tsl(educ[e])) then
                    qae[e]:=tsl[no_tsl(educ[e])].qatm
                 else
                     qae[e]:=0;

             for e:=1 to 2 do          // cycle through educts
              // current educt equation
              if in_tsl(educ[e]) then
                 for k:=1 to _isos do  // cycling through isotopologues
{$IFDEF DBL_EXPL}
                  for x:=1 to _isos do // another cycle for explicit doubling
{$ENDIF}
                    begin

                    a:=' <';
                    a+=substr(drsyntax,'@',abbr)+clsname[k];
                    if (if2t) then                       // in case both educts are tagged
{$IFNDEF DBL_EXPL}
                       if (educ[1]<>educ[2]) then
                          a+='i'+educ[e]                 // impl. quad. nomenclature: G4110I12CiSRC
                       else
                           a+='i2'+educ[e];              // impl. quad. nom. for same educts: G4110I12Ci2SRC
{$ELSE}
                       a+='e'+inttostr(x);               // expl. quad. nomenclature: G4215aI13Ce2
{$ENDIF}
                    a+='> ';

                    a+=giveiso(educ[e],k);               // isotopologue
                    if (educ[3-e]<>'') then              // '3-e' gives 2(e=1), 1(e=2)
{$IFNDEF DBL_EXPL}
                       a+=' + '+educ[3-e];               // 2nd educt,  impl.: x=0 => giveiso returns "regular" spec
{$ELSE}
                       a+=' + '+giveiso(educ[3-e],x);    // explicit: corresp. other educt isotopologue
{$ENDIF}
                    a+=' = ';                            // eq sign
{$IFNDEF DBL_EXPL}
                    a+=educ[3-e];                        // replicating "regular" @right side
{$ENDIF}

                    // accounting the atom fraction of current educt: account in stoi`s of the products
                    af:=qae[e]/(qae[1]+qae[2]);          // this is a place for improvement (now using source subs)!

                    for j:=1 to _prod do     // cycling products
                        begin
                        savstoi:=stoi[j];
{$IFNDEF DBL_EXPL}
                        if (in_tsl(prod[j])) then        // filtering out all non-tagged species in implicit
{$ENDIF}
                           begin
                           if (a[length(a)-1]<>'=') then a+=' + '; // if right side is not empty
{$IFNDEF DBL_EXPL}
                           stoi[j]:=stoi[j]*af;          // quadruplicated equations
{$ELSE}
                       ???       stoi[j]:=stoi[j]/_isos; // _isos-replicated equations (explicit doubling case)
{$ENDIF}
                           // accounting the transfer of the minor isotope atom
                           // from educt molecule to current product molecule
                           b:='';
                           if (in_tsl(prod[j]) and (k<>1)) then        // if reaction is for the minor isotopologue transfer
                              begin
                              qap:=tsl[no_tsl(prod[j])].qatm;          // qty. of atoms in product
                              pf:=qap/qae[e];                          // prob = A(prod)/A(educ), i.e. 1/3 for C3H6 -> CH3O2

                              // if # of atoms in product = # atoms in educt: straightforwardly creating minor isotopologue
                              // if # of atoms in product > # atoms in educt: adding atoms from major pool
                              // if # of atoms in product < # atoms in educt: freeing abundant excess to the major pool
                              if not(pf=1.0) then
                                 begin
                                 if ((stoi[j]*(1.0-pf))<>1.0) then
                                    b+=formatfloat(stoiformat,stoi[j]*(1.0-pf)); // coeff. for a major isotopol. in minor reac
                                 if ((1.0-pf)>0) then
                                    b:='+'+b;                             // insert "+" in case of positive stoi coeff
                                 insert(' ',b,2);                         // insert space between stoi coeff and its sign
                                 b:=' '+b+' ';                            // stylish?
                                 b+=giveiso(prod[j],1);                   // major isotopologue
                                 stoi[j]:=stoi[j]*pf;                     // accounting minor transfer prob. in stoi
                                 end;
                              end;

                           if (stoi[j]<>1.0) then                           // placing <>1 stoi coefficient
                              a+=formatfloat(stoiformat,stoi[j])+' '; // by trial&error found: MECCA makes no difference in
                           a+=giveiso(prod[j],k);                           //   precision starting from 12 dig. after the comma

                           a+=b;
                           end;

                        // adding production PT
                        if (in_bsl(prod[j])) then
                           begin
                           a+=' + ';
                           if (stoi[j]<>1.0) then
                              a+=formatfloat(stoiformat,stoi[j])+' ';
                           a+=substr(ptpsyntax,'@',clsname[k]+prod[j]);
                           end;

                        // reverting changes to the stoichiom. coeff
                        stoi[j]:=savstoi;
                        end;

                    // loss PTs
                    for i:=1 to 2 do
                        if (in_bsl(educ[i])) then
                           begin
                           a+=' + ';
                           if (if2s) then a+='2 ';
                           a+=substr(ptlsyntax,'@',clsname[k]+educ[i]);
                           end;

                    // checking if there are no produts on the right side (USE_PT off & destruction to nothing - quite possible)
                    b:=trim(a); if (b[length(b)]='=') then a:=a + 'Dummy ';
{$IFDEF USE_PT}

 {$IFDEF DBL_EXPL}
                    // PTs in explicit case: all equations have the same PT - total production
                    if (pos(substr(ptsyntax,'@',abbr),eqnfile[l].line)=0) then
                       // adding production tracer
                       a+=' + '+substr(ptsyntax,'@',abbr);
 {$ENDIF}

 {$IFDEF USE_PT_KIE}
                    // add passive tracer to the the reaction which has kie
                    for j:=1 to _kie do
                        if (kie[j].imec) and (abbr=kie[j].abbr) then
                           begin
                           a+=' + '+substr(ptsyntax,'@',abbr+clsname[k]);
  {$IFNDEF DBL_EXPL}
                           if (if2t) then a+='i'+inttostr(e);
  {$ELSE}
                           if (if2t) then a+='e'+inttostr(x);
  {$ENDIF}
                           break;
                           end;
 {$ENDIF}
{$ENDIF}

                    // adding reaction rate
{$IFNDEF USE_DKRATE}
                    a+=' :'+phys;
{$ELSE}
                    a+=' :'+' {%*} '+substr(dkratesyntax,'@',abbr)+'; {&&}';
{$ENDIF}

                    // in case educts are equal, skip duplicating reaction but doubling the rate
                    if (if2s) then
                       if (e=1) then
                          insert('*(2.0)',a,pos(';',a))
                       else
                           continue;

                    // checking whether the reaction has kie
                    for j:=1 to _kie do
                        if (kie[j].imec) and (abbr=kie[j].abbr) then
                           if (kie[j].isot=tsl[no_tsl(educ[e])].isos[k]) then
                              begin
                              insert('( ',a,pos('}',a)+2);            // adding left brace
                              insert(' )'+kie[j].expr,a,pos(';',a));
                              // adding right brace and expr. to the end of phys line before ;
                              end;

                    writeln(f,a);
                    end;

             end
          else   // for 'if not(isrc) and not(iiex)'
              if (isrc) then
                 // if products have no sources in .eqn (source specification)
                 with src[eqs[eqnfile[l].eqno].nsrc] do
                      for e:=1 to _trans do                 // e cycles through source specification
                          if (trans[e]._dst>0) then         // avoiding sources without destination
                             begin

                             // transfer info string
                             a:='// <'+abbr+'> '+trans[e].src+' = ';
                             for j:=1 to trans[e]._dst do
                                 begin
                                 if not(trans[e].dst[j].stoi=1.0) then
                                    a+=formatfloat('##0.###',trans[e].dst[j].stoi)+' ';
                                 a+=trans[e].dst[j].spec;
                                 if (j<trans[e]._dst) then
                                    a+=' + ';
                                 end;
                             a+=' : ; {%isotrans:'+isoatom+'}';
                             writeln(f,a);       

                             // if a specified source is one of the educts given in the eq, creating normal r-n
                             // else creating futile destruction r-n
                             d:=0;                  // no of detected educt matching src specification
                             for j:=1 to 2 do
                                 if in_tsl(educ[j]) then
                                    if (educ[j]=trans[e].src) then
                                       begin
                                       d:=j; break;
                                       end;

                             // products
                             for k:=1 to _isos do
                                 begin

                                 // composing reaction
                                 // educts
                                 a:=' <'+substr(drsyntax,'@',abbr)+clsname[k]+'s'+trans[e].src+'> ';
                                 if (d>0) then j:=d else j:=1;
                                 if (educ[3-j]<>'') then
                                    a+=educ[3-j]+' + ';
                                 if (d>0) then a+=giveiso(educ[d],k)
                                          else a+=educ[j];
                                 a+=' = ';
                                 if (educ[3-j]<>'') then
                                    a+=educ[3-j];
                                 if (d=0) then a+=' + '+educ[j];

                                 // q-ty of atoms in educts
                                 qae[1]:=tsl[no_tsl(trans[e].src)].qatm;
                                 qae[2]:=0;                // source specification: contibution of only one educt

                                 i:=length(a); // storing the length of eq to determine if any product is written ot not
                                 for j:=1 to _prod do
                                     begin
                                     savstoi:=stoi[j];
{$IFNDEF DBL_EXPL}
                                     if (in_tsl(prod[j])) then
{$ENDIF}
                                        begin

                                        // should be accounted for in current (e) source?
                                        if ( no_trans_dst(eqs[eqnfile[l].eqno].nsrc,e,prod[j])>0 ) then
                                           begin
                                           a+=' + ';  // for each product

                                           // accounting the number of total source specification(s) for this species
                                           stoi[j]:=stoi[j]*trans[e].dst[no_trans_dst(eqs[eqnfile[l].eqno].nsrc,e,prod[j])].stoi*trans[e].weight;

                                           // accounting the transfer of the minor isotope atom
                                           // from educt molecule to current product molecule
                                           b:='';
                                           if (in_tsl(prod[j]) and (k<>1)) then        // if reaction is for the minor isotopologue transfer
                                              begin
                                              qap:=tsl[no_tsl(prod[j])].qatm;          // qty. of atoms in product
                                              pf:=qap/qae[1];                          // prob = A(prod)/A(educ), i.e. 1/3 for C3H6 -> CH3O2

                                              // if # of atoms in product = # atoms in educt: just creating minor isotopologue
                                              // if # of atoms in product > # atoms in educt: adding atoms from major pool
                                              // if # of atoms in product < # atoms in educt: freeing abundant excess to the major pool
                                              if not(pf=1.0) then
                                                 begin
                                                 if ((stoi[j]*(1.0-pf))<>1.0) then
                                                    b+=formatfloat(stoiformat,stoi[j]*(1.0-pf)); // coeff. for a major isotopol. in minor reac
                                                 if ((1.0-pf)>0) then
                                                    b:='+'+b;                             // insert "+" in case of positive stoi coeff
                                                 insert(' ',b,2);                         // insert space between stoi coeff and its sign
                                                 b:=' '+b+' ';                            // stylish?
                                                 b+=giveiso(prod[j],1);                   // major isotopologue
                                                 stoi[j]:=stoi[j]*pf;                     // accounting minor transfer prob. in stoi
                                                 end;
                                              end;

                                           if (stoi[j]<>1.0) then
                                              a+=floattostr(stoi[j])+' ';
                                           a+=giveiso(prod[j],k);

                                           a+=b;
                                           end;

                                        // adding production PT
                                        if (in_bsl(prod[j])) then
                                           begin
                                           a+=' + ';
                                           if (stoi[j]<>1.0) then
                                              a+=formatfloat(stoiformat,stoi[j])+' ';
                                           a+=substr(ptpsyntax,'@',clsname[k]+prod[j]);
                                           end;

                                        // reverting changes to the stoichiom. coeff
                                        stoi[j]:=savstoi;
                                        end;

                                     end;

                                 if (i=length(a)) then // no products were written, i.e. src -> empty
                                    continue;

                                 if (d>0) then
                                 // loss PTs
                                 for i:=1 to 2 do
                                    if (in_bsl(educ[i])) then
                                       begin
                                       a+=' + ';
                                       if (if2s) then a+='2 ';
                                       a+=substr(ptlsyntax,'@',clsname[k]+educ[i]);
                                       end;

{$IFDEF USE_PT}
 {$IFDEF DBL_EXPL}
                                 // PTs in explicit case
                                 if (pos(substr(ptsyntax,'@',abbr),eqnfile[l].line)=0) then
                                    // adding production tracer
                                    a+=' + '+substr(ptsyntax,'@',abbr);
 {$ENDIF}
 {$IFDEF USE_PT_KIE}
                                 // add passive tracer to the the reaction which has kie
                                 for j:=1 to _kie do
                                     if ((kie[j].imec) and (abbr=kie[j].abbr)) then
                                        begin
                                        a+=' + '+substr(ptsyntax,'@',abbr+clsname[k]);
                                        break;
                                        end;
 {$ENDIF}
{$ENDIF}
                                 // adding reaction rate
{$IFNDEF USE_DKRATE}
                                 a+=' :'+phys;
{$ELSE}
                                 a+=' :'+' {%*} '+substr(dkratesyntax,'@',abbr)+'; {&&}';
{$ENDIF}
                                 // weighting reaction rate with corresponding isotopomer fraction
                                 //   if the source is not the one from the left side
                                 if (d=0) then
                                    begin
                                    insert('( ',a,pos('}',a)+2); // adding left brace
                                    insert(' )*'+substr(dsrcsubsyntax,'@',clsname[k]+trans[e].src),a,pos(';',a));
                                    end;

                                 // in case educts are equal, skip duplicating reaction but doubling the rate
                                 if ((d>0) and (educ[1]=educ[2])) then
                                    insert('*(2.0)',a,pos(';',a));

                                 // checking whether the reaction has kie
                                 for j:=1 to _kie do
                                     if (abbr=kie[j].abbr) then
                                        if (kie[j].isot=tsl[no_tsl(trans[e].src)].isos[k]) then
                                           begin
                                           insert('( ',a,pos('}',a)+2); // adding left brace
                                           insert(' )'+kie[j].expr,a,pos(';',a));
                                           // adding right brace and expr. to the end of phys line before ;
                                           end;

                                 writeln(f,a);

                                 end;

                             end
                          else   // 'if (trans[e]._dst>0)'
              else   // 'if (not(isrc)'
                  if (iiex) then
                     begin
             
                     // isotope exchange reaction

                     // educts
                     for e:=1 to 2 do
                       if (_isos>1) then // multiple istopologues
                         for k:=2 to _isos do
                           with iex[niex] do
                             begin

                             // new r-n abbr
                             a:=' <'+substr(drsyntax,'@',abbr)+tsl[exspec[e]].isos[k]+'> ';

                             a+=tsl[exspec[e]].isos[k]  +' + ';    // major
                             a+=tsl[exspec[3-e]].isos[1]+' = ';    // rare

                             // exchanging one rare isotope atom
                             a+=tsl[exspec[e]].isos[1]+' + ';      // now rare
                             a+=tsl[exspec[3-e]].isos[k];          // now major
            
                             // rate
{$IFNDEF USE_DKRATE}
                             a+=' :'+phys;
{$ELSE}
                             a+=' :'+' {%*} '+substr(dkratesyntax,'@',abbr)+'; {&&}';
{$ENDIF}
                             // checking whether the reaction has kie
                             for j:=1 to _kie do
                                 if (kie[j].imec) and (abbr=kie[j].abbr) then
                                    if (kie[j].isot=tsl[exspec[e]].isos[k]) then
                                       begin
                                       insert('( ',a,pos('}',a)+2);            // adding left brace
                                       insert(' )'+kie[j].expr,a,pos(';',a));
                                       // adding right brace and expr. to the end of phys line before ;
                                       end;

{$IFNDEF IEX_ONWARD_RATES}
                             // calculating transfer probability (as nu = q(3-e) / (q(1)+q(2)))
                             // referred to the regular r-n rate, for both reactants, both directions
                             j:=tsl[exspec[1]].qatm+tsl[exspec[2]].qatm; // total on the left side
                             insert('('+inttostr(tsl[exspec[3-e]].qatm)+'.0/'+inttostr(j)+'.0)*',a,pos('}',a)+2);
{$ELSE}
                             // calculating transfer probability (as nu = q(3-e) / q(e))
                             // referred to the onward r-n rate, reverse direction
                             if (e=2) then
                                insert('('+inttostr(tsl[exspec[3-e]].qatm)+'.0/'+inttostr(tsl[exspec[e]].qatm)+'.0)*',a,pos('}',a)+2);
{$ENDIF}

                             // voila!
                             writeln(f,a);

                             end
                       else // "fractional" isotopologues (i.e. one)
                           with iex[niex] do
                             begin

                             // new r-n abbr
                             a:=' <'+substr(drsyntax,'@',abbr)+tsl[exspec[e]].isos[k]+'> ';
                             a+=tsl[exspec[e]].isos[1]  +' + ';  // minor disappeared
                             a+=tsl[exspec[3-e]].spec+' = ';     // regular (minor+rest)

                             // exchanging one rare isotope atom
                             a+=tsl[exspec[3-e]].spec+' + ';     // replicationg regular (minor+rest)
                             a+=tsl[exspec[3-e]].isos[1];        // minor appeared
            
                             // rate
                             //   add. modified acc. to the fraction of the major
{$IFNDEF USE_DKRATE}
                             a+=' :'+phys;
{$ELSE}
                             a+=' :'+' {%*} '+substr(dkratesyntax,'@',abbr)+
{$IFDEF PRECISE_FISO_IEX}
                                     '*(1.0 - '+substr(dsrcsubsyntax,'@',clsname[1]+tsl[exspec[3-e]].spec)+')'+
{$ENDIF}
                                     '; {&&}';
{$ENDIF}
                             // checking whether the reaction has kie
                             for j:=1 to _kie do
                                 if (kie[j].imec) and (abbr=kie[j].abbr) then
                                    if (kie[j].isot=tsl[exspec[e]].isos[k]) then
                                       begin
                                       insert('( ',a,pos('}',a)+2);            // adding left brace
                                       insert(' )'+kie[j].expr,a,pos(';',a));
                                       // adding right brace and expr. to the end of phys line before ;
                                       end;

{$IFNDEF IEX_ONWARD_RATES}
                             // calculating transfer probability (as nu = q(3-e) / (q(1)+q(2)))
                             // referred to the regular r-n rate, for both reactants, both directions
                             j:=tsl[exspec[1]].qatm+tsl[exspec[2]].qatm; // total on the left side
                             insert('('+inttostr(tsl[exspec[3-e]].qatm)+'.0/'+inttostr(j)+'.0)*',a,pos('}',a)+2);
{$ELSE}
                             // calculating transfer probability (as nu = q(3-e) / q(e))
                             // referred to the onward r-n rate, reverse direction
                             if (e=2) then
                                insert('('+inttostr(tsl[exspec[3-e]].qatm)+'.0/'+inttostr(tsl[exspec[e]].qatm)+'.0)*',a,pos('}',a)+2);
{$ENDIF}

                             // voila!
                             writeln(f,a);
                             end;

                     end;         // if (iiex)

          end
       else // 'if (...itag)'
           writeln(f,eqnfile[l].line);        // just output the equation which is not tagged

       // continuing within 'if (...iseq)' block

       // writing reactions to the end of the main block
       
       if (eqnfile[l].eqno=_eqs) then
          begin
          // LAST dummy reaction here for totals (Ti) so MECCA won't throw it away
          // et la reaction futile
          writeln(f);
          writeln(f,'{------ [imdouble] - total ',isoatom,' dummy reaction ----------------------------------}');
          // assuming there is at least two isotopologues (!)
          write(f,'<D0000T',isoatom,'> '+cfgname+'T'+isoatom,' = ',
                                         cfgname+'T'+clsname[1]);
          for j:=2 to _isos do
              write(f,' + '+cfgname+'T'+clsname[j]);
          writeln(f,' : {%StTrG}  0.0;  {&&}');
{$IFDEF DBL_EXPL}
          // les reactions futiles pour les substances ordinaires
          // here assumed that there are at least 3 three tagged species in the mech
          writeln(f,'{------ [imdouble] - regular species dummy reactions -------------------------}');
          write('  DBL_EXPL dummy reactions: ');
          for k:=0 to ((_utsl-1) div 10) do
              begin
              write(k,' ');
              i:=  (k)*10+1;
              j:=(k+1)*10;
              if (j>_utsl) then
                 begin
                 // dec(i,j-_utsl);
                 dec(j,j-_utsl);
                 i:=j-(j mod 10)+1; if ((i+2)>_utsl) then i:=_utsl-2;
                 if (j<1) or (i<1) then
                    begin
                    writeln(' problem creating dummy reactions for regular species for explicit case. stop.');
                    halt(1);
                    end;
                 end;
              write(f,'<G000',k,'E> ',tsl[utsl[i]].spec,' + ',tsl[utsl[i+1]].spec,
                                                        ' = ',tsl[utsl[i+2]].spec);
              for x:=i+3 to j do
                  write(f,' + ',tsl[utsl[x]].spec);
              writeln(f,' : {%StTrG}  0.0;  {&&}');
              end;
          writeln;
{$ENDIF}
          end
       end    // 'if (eqnfile[l].iseq)' block
else
    begin
    // adding some code before equations
    if (pos('#EQUATIONS',eqnfile[l].line)>0) then
       begin
{$IFDEF USE_DKRATE}
       addinline4dblrates;                 // if rates calculation optimization is on (see USE_DKRATE CD)
{$ENDIF}

       if (_src>0) then                    // if source specification takes place
          addinline4subs;

       if (kieproc<>_nonestr) then         // if there is KIE [rocessing file specified
          begin
          writeln(f,'{------ [imdouble] - KIE process section for ',isoatom,' -------------------------------}');
          write(f,imcom_parse_proc(kieproc));   // kie processing for doubling
          writeln(f,'{------ [imdouble] -----------------------------------------------------------}');
          end;

       end;

       writeln(f,eqnfile[l].line);
    end;

writeln(f);
writeln(f);

close(f);

writeln('done');

end;

// -----------------------------------------------------------------

// additional species list - interconfigurational
// synx: :I12CO=C12 + O>
var addspecs_intr : ansistring;
   _addspecs_intr : integer;

procedure imdouble_update_addspecs;
var i, j : integer;
begin
for i:=1 to _utsl do
    with tsl[utsl[i]] do
         for j:=1 to _isos do
             begin
             addspecs_intr+=':'+clsname[j]+spec+'>';
             inc(_addspecs_intr);
             end;
addspecs_intr+=':'+cfgname+'T'+isoatom+'>';
for i:=1 to _isos do
    addspecs_intr+=':'+cfgname+'T'+clsname[i]+'>';
inc(_addspecs_intr,1+_isos);

writeln('imdouble_update_addspecs: done [total ',_addspecs_intr,' specs]');
end;


// additional species list
procedure imdouble_produce_speciesfile(fname : string);

var f : textfile;
    i, j, k : word;
    declspecs, a : ansistring;

// add species checking for existing in the present species list
procedure safeaddspec(name, data : string);
begin
if (in_spc(name)) then declspecs+=name+' '
                  else writeln(f,'  ',name,' = ',data);
end;


begin

write('produce_speciesfile(',fname,'): ');
declspecs:='';

assign(f,fname);
rewrite(f);

// filling in previous species file
for i:=1 to _spcfile do
    writeln(f,spcfile[i].line);

writeln(f);
writeln(f,'{-------------- [imdouble] ---------------------------------------------------}');
writeln(f);
writeln(f,'{ additional species list for doubled mechanism based on: ',paramstr(1),' }');
writeln(f,'{ configuration: ',cfgname,' }');
writeln(f,'{ intended # of species to add: ',_addspecs_intr,' }');
{$IFDEF USE_PT}
writeln(f,'{ intended # of PTs to add: ',_ptracs_intr,' }');
{$ENDIF}
writeln(f);
writeln(f,'{ =',datetimetostr(now),'= }');
writeln(f);

writeln(f,'{- new atoms -----------------------------------------------------------------}');
writeln(f);
writeln(f,'#ATOMS');
writeln(f);
for i:=1 to _isos do
    if (isomass[i]>0) then
       writeln(f,'  ',clsname[i],'     { mass ',isomass[i]:0:7,' of element ',isoatom,' };')
    else
       writeln(f,'  ',clsname[i],'     { species class ',clsname[i],'(',i,') };');
writeln(f);

writeln(f,'{- doubled species -----------------------------------------------------------}');
writeln(f);

a:='';

(* this method is no longer used since the introduction of spc[] and work with species file
tmp:=addspecs_intr;
while (length(tmp)>0) do
      begin
      // one addspecs entry is like :C5H8>

      a:=copy2symbdel(tmp,':');   // rem ':'
      // delete(tmp,1,1); // uncomment this for fp package earlier than 5.2.0

      a:=copy2symbdel(tmp,'>');   // copy to '>'
      // delete(tmp,1,1); // uncomment this for fp package earlier than 5.2.0

      writeln(f,'  ',a,' = IGNORE;');
      end; *)

writeln(f,'#DEFVAR');
writeln(f);
for i:=1 to _utsl do
    with tsl[utsl[i]] do
         begin
         
         for j:=1 to _isos do
             safeaddspec(isos[j],spc[nspc].icomp[j]+'; '+spc[nspc].icapt[j]+'   { '+spec+' '+clsname[j]+isoatom+' isotopologue }');
          
         writeln(f);
         end;

writeln(f,'{ budgeting PTs }');
writeln(f);
for i:=1 to _bsl do
    with bsl[i] do
         begin
         
         // loss PTs
         if (iloss) then
            for j:=1 to _isos do
                safeaddspec( substr(ptlsyntax,'@',clsname[j]+spec),{spc[nspc].icomp[j]}'IGNORE'+'; { '+spec+' '+clsname[j]+' loss PT }');
         // production PTs
         if (iprod) then
            for j:=1 to _isos do
                safeaddspec( substr(ptpsyntax,'@',clsname[j]+spec),{spc[nspc].icomp[j]}'IGNORE'+'; { '+spec+' '+clsname[j]+' production PT }');
         
         writeln(f);
         end;


writeln(f,'{ totals }');
writeln(f);
safeaddspec(cfgname+'T'+isoatom,'IGNORE; { '+cfgname+' total '+isoatom+' atoms count }');
for j:=1 to _isos do
    safeaddspec(cfgname+'T'+clsname[j],'IGNORE; { '+cfgname+'total '+clsname[j]+isoatom+' atoms count }');
writeln(f);

{$IFDEF USE_PT}
writeln(f,'{- passive production tracers ------------------------------------------------}');
writeln(f);

for i:=1 to _eqs do
    with eqs[i] do
         if (itag) then
            safeaddspec(substr(ptsyntax,'@',abbr),'IGNORE; { '+abbr+' reaction passive production tracer }');
writeln(f);

(* this method is no longer used since the introduction of spc[] and work with species file
tmp:=ptracs_intr;
while (length(tmp)>0) do
      begin
      // one ptracs entry is like :G4410>

      a:=copy2symbdel(tmp,':');   // rem ':'
      // delete(tmp,1,1); // uncomment this for fp package earlier than 5.2.0

      a:=copy2symbdel(tmp,'>');   // copy to '>'
      // delete(tmp,1,1); // uncomment this for fp package earlier than 5.2.0

      writeln(f,'  ',substr(ptsyntax,'@',a),' = IGNORE; { ',a,' reaction production }');
      end; *)

 {$IFDEF USE_PT_KIE}
// in case of changes refer to the imcom_update_ptracs section in imcom.inc
for k:=1 to _kie do
    if (kie[k].imec) then       // if KIE exist in this configuration
       with kie[k] do
            for j:=1 to _isos do
 	        if not( in_tsl(eqs[kie[k].eqno].educ[1]) and in_tsl(eqs[kie[k].eqno].educ[2]) ) then    // not a quadrupl. reaction
		   safeaddspec(abbr+clsname[j],'IGNORE; { '+abbr+' reaction '+isot+' KIE production tracer }')
                else
  {$IFNDEF DBL_EXPL}
		    begin	    // in case both educts are tagged (quadrupled equation = quad. kie PTs)
		    safeaddspec(kie[k].abbr+clsname[j]+'i1',
                                'IGNORE; { '+abbr+' reaction '+isot+' KIE production tracers }');
		    safeaddspec(kie[k].abbr+clsname[j]+'i2',
                                'IGNORE;');
                    end;
  {$ELSE}
		    safeaddspec(kie[k].abbr+clsname[j]+'e'+inttostr(1),
                                'IGNORE; { '+abbr+' reaction '+isot+' KIE production tracers }');
                    for i:=2 to _isos do  // in case of explicit doubling (_iso-replicated equations with KIE)
		        safeaddspec(kie[k].abbr+clsname[j]+'e'+inttostr(i),'IGNORE;');
  {$ENDIF}

writeln(f);
 {$ENDIF}
{$ENDIF}

{$IFDEF ADD_DUMIND}
writeln(f,'{- indices of species not tagged but still present in the TSL -----------------}');
writeln(f);

for i:=1 to _tsl do
    if not(is_usedspec(tsl[i].spec)) then
       with tsl[i] do
            begin
         
            for j:=1 to _isos do
                safeaddspec(isos[j],'IGNORE; { dummy isotopologue }');
            writeln(f);

            end;
{$ENDIF}

writeln(f,'{-------------- [imdouble] - end ---------------------------------------------}');
writeln(f);

close(f);

if (declspecs<>'') then write('(warning, declined as already existing species: ',declspecs,') ');

writeln('done');

exit;

(*
for i:=1 to _utsl do
    begin
    for j:=1 to _isos do
        writeln(f,'  ',tsl[utsl[i]].isos[j],' = IGNORE; { ',tsl[utsl[i]].spec,' isotopologue }');
    writeln(f);
    end;

writeln(f,'{ ! following species were not found in the selected mechanism: }');
  write(f,'{ ! ');
for i:=1 to _tsl do
    if not(is_usedspec(tsl[i].spec)) then
       write(f,tsl[i].spec,' ');
writeln(f,'}');
writeln(f);

writeln(f,'  T',isoatom,'   = IGNORE; { total ',isoatom,' }');
for i:=1 to _isos do
    writeln(f,'  '+tagabbr+clsname[i]+'T'+isoatom,' = IGNORE; { total ',clsname[i],isoatom,' }');

writeln(f);

{$IFDEF USE_PT}
writeln(f,'{- passive production tracers ------------------------------------------------}');
writeln(f);

{$IFNDEF DBL_EXPL}
for i:=1 to _eqs do
    if (eqs[i].itag) then
       writeln(f,'  PT',eqs[i].abbr,' = IGNORE; { ',eqs[i].abbr,' reaction production }');
writeln(f);
{$ENDIF}

{$IFDEF USE_PT_KIE}        // if monitoring KIE, all isotopologues
a:='';                         // a has the list of reactions for which PTs are created
writeln(f,'{ ! reactions with KIE }');
for i:=1 to _kie do
    for k:=1 to _eqs do
        if (eqs[k].itag) then
           if ((eqs[k].abbr=kie[i].abbr) and (pos('>'+eqs[k].abbr+'<',a)=0)) then
              begin
              for j:=1 to _isos do
{$IFNDEF DBL_EXPL}
                  // in case only one educt is tagged
                  if not(in_tsl(eqs[k].educ[1]) and in_tsl(eqs[k].educ[2])) then
                     writeln(f,'  PT',kie[i].abbr+tagabbr+clsname[j]+isoatom,' = IGNORE; { ',
                               kie[i].abbr,' reaction production for ',clsname[j],isoatom,' }')
                  else
                      // in case both educts are tagged (quadrupled equation = quad. kie PTs)
                      begin
                      writeln(f,'  PT'+kie[i].abbr+tagabbr+clsname[j]+isoatom+'x = IGNORE; { ',
                                       kie[i].abbr,' reaction production for ',clsname[j],isoatom,' (quad, 1st pair) }');
                      writeln(f,'  PT'+kie[i].abbr+tagabbr+clsname[j]+isoatom+'y = IGNORE; { ',
                                       kie[i].abbr,' reaction production for ',clsname[j],isoatom,' (quad, 2nd pair) }');
                      end;
{$ELSE}
                  // in case of explicit doubling and source specification create only _isos number of kie PTs
                  if  (eqs[k].isrc) then
                      writeln(f,'  PT',kie[i].abbr+tagabbr+clsname[j]+isoatom,' = IGNORE; { ',
                                       kie[i].abbr,' reaction production for ',clsname[j],isoatom,' (source specification) }')
                  else
                      if not(in_tsl(eqs[k].educ[1]) and in_tsl(eqs[k].educ[2])) then  // single tagged educt
                         writeln(f,'  PT'+kie[i].abbr+tagabbr+clsname[j]+isoatom+' = IGNORE; { ',
                                          kie[i].abbr+' reaction production for '+clsname[j]+isoatom+' }')
                      else
                          for x:=1 to _isos do                                        // both educts are tagged
                              writeln(f,'  PT'+kie[i].abbr+tagabbr+clsname[j]+isoatom+'e',x,' = IGNORE; { ',
                                           kie[i].abbr,' reaction production for ',
                                           clsname[j],isoatom,' (explicit ',x,' pair) }');
{$ENDIF}
              a+='>'+eqs[k].abbr+'<'+':';
              end;
writeln(f);
{$ENDIF}

{$ENDIF}

writeln(f,'{-------------- [imdouble] - end ---------------------------------------------}');
writeln(f);

close(f);

writeln('done');
*)
end;


// -----------------------------------------------------------------

// some useful code lines for boxmodel
procedure produce_imdouble_code(tname, fname : string);

var f : textfile;



// main proc ----------------------------------------------------------

var t, finc : text;  // template, include file
    a, u, p, s : ansistring;
    ec : boolean;    // "empty" configuration flag

// main part
begin

write('produce_imdouble_code(',fname,'): ');

// filling replacements
imcom_update_reps;

imcom_check_files_exist([tname, kieproc]);

// checking for an empty configuration
if (cfgname='') then ec:=true else ec:=false;

// output code
assign(f,fname);
rewrite(f);

// template file
assign(t,tname);
reset(t);

while not(eof(t)) do
      begin

      readln(t,a);
      imcom_make_reps(a);

      if pos('{>',a)>0 then
         begin
         // get control property and value letter    (e.g. ATOM:C or ISO:2 or SPEC:Ch3Br etc.)
         p:=upcase(trim(imcom_ext4marks(a,'{>',':')));
         u:=trim(imcom_ext4marks(a,':','}'));

         if not(imcom_condition(p,u)) then
            repeat
                  readln(t,a);
            until ( ((pos(('{<'),a)>0) and (pos(uppercase(p),uppercase(a))>0)) or (eof(t)) );
         end
      else
          if pos('{<',a)>0 then
             begin {do nothing} end
          else
              if pos('{$DBL_INFO',a)>0 then
                 begin
                 writeln(f,'! ',eqnfile[1].line);
                 writeln(f,'!     Source mech. equations file: ',paramstr(1),' (',_eqs,' reactions)');
                 writeln(f,'! # of tagged reactions/added PTs: ',nooftagreac);
                 writeln(f,'!        Tagged species list file: ',paramstr(2),' (',_utsl,' of ',_tsl,' given species tagged)');
                 end
              else
          if pos('{$CONF_PARAM',a)>0 then
             writeln(f,imcom_parameters)
          else
          if not(ec) and (pos('{$CONF_LIST',a)>0) then
             writeln(f,imcom_make_configslist(imcom_ext4marks(a,'[%','%]')))
          else
          if pos('{$TRAC_DECL',a)>0 then
                 writeln(f,imcom_make_tracdecl(imcom_ext4marks(a,'[%','%]')))
          else
          if pos('{$TAG_SPECS',a)>0 then
             writeln(f,imcom_make_tagspecs_list(imcom_ext4marks(a,'[%','%]')))
          else
          if pos('{$x0',a)>0 then            // parameters are: classno, initexpression
             writeln(f,imcom_make_x0(imcom_ext4marks(a,'[%','%]'),
                                     imcom_ext4marks(a,'(%','%)')))   //_imdouble_x0
          else
          if pos('{$RESET_PTs',a)>0 then
             write(f,imcom_make_resetPTs(imcom_ext4marks(a,'[%','%]'),
                                         imcom_ext4marks(a,'(%','%)')))
          else
          if (pos('{$ELSA',a)>0) then
             writeln(f,wrap(imcom_parse_eq_strarr_len(copy(a,pos('}',a)+1,length(a)-pos('}',a))),7,7))
          else
          if (pos('{$INCLUDE',a)>0) then
             begin
             s:=imcom_ext4marks(a,'<','>');
             if (fileexists(s)) then
                begin
                assign(finc,s);
                reset(finc);
                while not(eof(finc)) do
                      begin
                      readln(finc,s);
                      imcom_make_reps(s);
                      writeln(f,s);
                      end;
                close(finc);
                end
             else
                 writeln('  $INCLUDE <',s,'>: file not found. skipping.');
             end
          else
              writeln(f,a);
      end;

close(t);

close(f);

//writeln;
writeln('done');

end;

// main

var i, nconf : integer;
    l_eqnfname, l_spcfname : string;   // eqn&spc filenames from the last processed configuration

begin

if (paramcount<3) then
   begin
   writeln('>> MECCA kinetic meccanism (isotopic) doubling');
   writeln('usage: ./imdouble <spcfile> <eqnfile> <tracdeffile> <tagging configuration(s) list>');
   writeln('   ex:             zzz.spc   yyy.eqn   xxx.tex       tagXX.cfg [tagXY.cfg ...]');
   halt;
   end;

writeln('[imdouble]');
writeln('=',datetimetostr(now),'=');
writebreak;

{$IFDEF DBL_EXPL}

SORRY, EXPLICIT VERSION NEEDS RE-FORMULATION AND RE-CODING NOW

if (paramcount>4) then
   begin
   writeln('sorry, stop. current version is EXPLICIT, compiled with a DBL_EXPL switch.');
   writeln('explicit doubling can be performed only for one configuration, please check input parameters');
   halt(1);
   end;
{$ENDIF}


// inter-conf. initialization
imcom_init;
_addspecs_intr:=0;
addspecs_intr:='';

_conf:=paramcount-3;

for nconf:=1 to _conf do
    begin

    writeln;
    writeln('>> doubling ',nconf,' of ',_conf,' configuration(s)...');
    writeln;

    imcom_check_files_exist([paramstr(nconf+3),paramstr(1),paramstr(2)]);

    // read tagging config
    imcom_read_tag_config(paramstr(nconf+3));
    
    // read species ans equations files interpreting according to the loaded config
    if (nconf=1) then
       begin
       l_spcfname:=paramstr(1);
       l_eqnfname:=paramstr(2);
       end
    else
        begin           // next configuration is based on previously created spc/eqn files
        l_eqnfname:=eqnfname;
        l_spcfname:=spcfname;
        end;

    imcom_read_spc(l_spcfname); 
    imcom_read_eqs(l_eqnfname);

    // skipping "empty" configuration
    if (cfgname='') then
       begin
       writeln;
       writeln('empty configuration detected, skipping doubling routines to code parsing');
       // copying input spc/eqn to destination spc/eqn
       shell('cp '+l_spcfname+' '+spcfname);
       shell('cp '+l_eqnfname+' '+eqnfname);
       continue;
       end;
    
    // updating inter-conf. PTs list
    imcom_update_ptracs;

    // updating inter-conf. additional specs. list
    imdouble_update_addspecs;
    
    // double the meccanism
    doubleit(eqnfname);                      // creating a new equation file
    imdouble_produce_speciesfile(spcfname);  // creating an additional specs file
    for i:=1 to _form_conf do
        produce_imdouble_code(form_conf[i,1],form_conf[i,2]);

    // check for a possible duplicate reactions
    imcom_check_eqn_dupes(eqnfname,'Dummy');

    conf[nconf]:=tagname;
    
    imcom_make_addtracdef(paramstr(3),tracdef);
    end;

writeln;
if (_form_intr>0) then writeln('inter-configuration formers:');
for i:=1 to _form_intr do
    produce_imdouble_code(form_intr[i,1],form_intr[i,2]);

writeln;
writeln('[imdouble]: done');
writeln;

end.
