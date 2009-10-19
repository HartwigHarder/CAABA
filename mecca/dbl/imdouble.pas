// en FPC v>=2.0.0

// = imdouble.pas ====================================================
//
// im-double main source module
// requires: all is done through imcom.inc
//
// [Gromov, MPIC, 2007-2008]
// ===================================================================

program imdouble;

// - conditional defines -------------------------------------------

{$DEFINE DBL}             // for imcom: compiling imdouble

{$DEFINE noDBL_EXPL}        // explicit doubling - original reactions are replaced
                          // can be defined during compilation with -dDBL_EXPL switch

{$DEFINE xUSE_PT}          // using passive tracers, may be OFF

{$DEFINE USE_PT_KIE}      // defines whether to add PTs to all
                          // KIE-reactions (for monitoring)

{$DEFINE USE_KRSIND}      // defines whether to output the list of
                          // KIE-related specs indices (for correction)

{$DEFINE USE_DKRATE}      // defines whether to put reaction rates in the eqn header as a
                          // separate variables and use them in multiple doubled reactions

// -----------------------------------------------------------------

//  linking common utils/types include file for im-double/tag
{$I imcom.inc}

// -----------------------------------------------------------------

// - DOUBLING PART -------------------------------------------------

{$IFDEF USE_DKRATE}
const prev_ptracs_intr : ansistring = ''; // previous value of ptracs_intr
      dkratesyntax = 'dbl_k#';
{$ENDIF}

// main doubling routine
procedure doubleit(fname : string; nconf : integer);

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
for i:=1 to _src do
    for j:=1 to src[i]._trans do
        begin
        gotcha:=false;
        for k:=1 to _srcspecs do
            if (src[i].trans[j].src=srcspecs[k]) then gotcha:=true;
        if (not(gotcha) and is_usedspec(src[i].trans[j].src)) then
           begin
           inc(_srcspecs);
           srcspecs[_srcspecs]:=src[i].trans[j].src;
           end;
        end;

writeln(f,'{-------------- [imdouble] - inline code: start ------------------------------}');

// GLOBAL part
writeln(f,'#INLINE F90_GLOBAL');
writeln(f,'! reaction rates modifiers according to the specified source isotopic composition');
if (nconf=1) then      // adding only once !
   writeln(f,'  REAL(dp) :: src_temp                 ! variable used for fractions calculation');

for i:=1 to _srcspecs do
    begin
//       writeln(f,'! '+src[i].spec+' isotopologues fractions');
    a:='  REAL(dp) :: ';
    for j:=1 to _isos do
        a:=a+'src_'+srcspecs[i]+'_f'+tagabbr+clsname[j]+isoatom+', ';
    setlength(a,length(a)-2);
    writeln(f,wrap(a,2,4));
    end;
writeln(f,'#ENDINLINE');

// RCONST part
writeln(f,'#INLINE F90_RCONST');
writeln(f,'! calculation of the isotopologues fractions');
//writeln(f,'!');

for i:=1 to _srcspecs do
    begin
    // total composition
    a:='(';
    for j:=1 to _isos do
        a:=a+'C(ind_'+tagabbr+clsname[j]+srcspecs[i]+')+';
    setlength(a,length(a)-1);
    a:=a+')';
    writeln(f,'  src_temp = ',a);
    writeln(f,'  IF (src_temp .GT. 0.0_dp) THEN');
    for j:=1 to _isos do
        writeln(f,'    src_'+srcspecs[i]+'_f'+tagabbr+clsname[j]+isoatom+' = ',
                    'C(ind_'+tagabbr+clsname[j]+srcspecs[i]+') / src_temp');
    writeln(f,'  ELSE');
    for j:=1 to _isos do
        writeln(f,'    src_'+srcspecs[i]+'_f'+tagabbr+clsname[j]+isoatom+' = 0.0_dp');
    writeln(f,'  ENDIF');
//  writeln(f,'!');
    end;
writeln(f,'#ENDINLINE');

(*
// RATES part
writeln(f,'#INLINE F90_RATES');
writeln(f,'  ELEMENTAL REAL FUNCTION zerodiv(what,by)');
writeln(f,'    ! safe division which gives 0 when /0 operation is performed');
writeln(f,'    REAL(dp), INTENT(IN) :: what, by   ! operands');
writeln(f,'    IF (by .EQ. 0.0) THEN');
writeln(f,'      zerodiv = 0.0');
writeln(f,'    ELSE');
writeln(f,'      zerodiv = REAL(what/by)');
writeln(f,'    ENDIF');
writeln(f,'  END FUNCTION zerodiv');
writeln(f,'#ENDINLINE');
writeln(f,'{-------------- [imdouble] - inline code: end --------------------------------}');
*)

end;

// creation of inline-part for updating of a
// rate constants coefficients for a doubled equations
procedure addinline4dblrates;
var i : word;
    a : ansistring;
begin

writeln(f,'{-------------- [imdouble] - inline code: start ------------------------------}');

// GLOBAL part
writeln(f,'#INLINE F90_GLOBAL');
writeln(f,'! reaction rates variables (to be used in multiplied doubled reactions)');
a:='  REAL(dp) :: ';
for i:=1 to _eqs do
    if (eqs[i].itag) then
       if (pos(':'+eqs[i].abbr+'>',prev_ptracs_intr)=0) then    // var. might have been added in the previous conf.
          a+=substr(dkratesyntax,'#',eqs[i].abbr)+', ';
setlength(a,length(a)-2);
writeln(f,wrap(a,2,4));

writeln(f,'#ENDINLINE');

// RCONST part
writeln(f,'#INLINE F90_RCONST');
writeln(f,'! reaction rates variables calculation (to be calculated once per rates evaluation)');
for i:=1 to _eqs do
    if (eqs[i].itag) then
       if (pos(':'+eqs[i].abbr+'>',prev_ptracs_intr)=0) then    // var. might have been added in the previous conf.
          writeln(f,'  '+substr(dkratesyntax,'#',eqs[i].abbr)+' = '+trim(imcom_ext4marks(eqs[i].phys,'}',';')+';'));
writeln(f,'#ENDINLINE');
writeln(f,'{-------------- [imdouble] - inline code: end --------------------------------}');

{$IFDEF USE_PT}
prev_ptracs_intr:=ptracs_intr;
{$ENDIF}
	  
end;

// - doubleit body

var i, j, k, l : integer;
    a : ansistring;

    e : integer;
    qae : array[1..2] of integer;   // quantity of atoms in educts
    qap : integer;                  //  -- " --             products
    b : ansistring;
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
  write(f,'! atom: ',isoatom,', ',_isos,' isotopologues of masses ');
for i:=1 to _isos do
    write(f,clsname[i],' ');
writeln(f,'}');
writeln(f,'! # of doubled / added species: ',_utsl,' / ',(_utsl+1)*_isos+1);
writeln(f,'! # of reactions in the selected mechanism: ',_eqs);
writeln(f,'! # of tagged reactions: ',nooftagreac,' (',_src,' subs)');
{$IFDEF USE_PT}
writeln(f,'! # of added passive tracers: ',_ptracs_conf,' (',_ptracs_conf-nooftagreac,' add.kie)');
{$ENDIF}
writeln(f,'! warning: current doubling is ',{$IFNDEF DBL_EXPL}'NOT ',{$ENDIF}'EXPLICIT');
writeln(f,'! PTs added: ',{$IFDEF USE_PT}'YES'{$ELSE}'NO'{$ENDIF},
             ', for KIE monitoring: ',{$IFDEF USE_PT_KIE}'YES'{$ELSE}'NO'{$ENDIF},'');
writeln(f,'! [Gromov, MPI-C] =', datetimetostr(now),'=');
writeln(f,'! --v-- further comes modified mechanism ----------------------------------');
writeln(f,'#ENDINLINE');
writeln(f);

for l:=1 to _eqnfile do
    if (eqnfile[l].iseq) then
       begin

       if (eqs[eqnfile[l].eqno].itag) then           // working with eqation or just passing line
          with eqs[eqnfile[l].eqno] do

          begin

{$IFNDEF DBL_EXPL}
          // skipping? creation of reaction for isotope exchange
          if (iiex) then write(f,'// ');

          // first, original eq with a production tracer added
          write(f,'<',abbr,'> ');

          // educts
          write(f,educ[1]);
          if (educ[2]<>'') then write(f,' + ',educ[2]);
          write(f,' = ');

          // products
          for j:=1 to _prod do
              begin
              if stoi[j]<>1.0 then
                 write(f,floattostr(stoi[j]),' ');
              write(f,prod[j]);
              if (j<_prod) then
                 write(f,' + ');
              end;
 {$IFDEF USE_PT}
          if (pos(substr(ptsyntax,'#',abbr),eqnfile[l].line)=0) then
             // adding production tracer
             write(f,' + '+substr(ptsyntax,'#',abbr));
 {$ENDIF}
          // adding reaction rate
 {$IFNDEF USE_DKRATE}
          writeln(f,' :'+phys);
 {$ELSE}
          writeln(f,' :'+' {%*} '+substr(dkratesyntax,'#',abbr)+'; {&&}');
 {$ENDIF}

{$ENDIF}

          // now the doubled reactions come

          // ok, is it a subsitution or normal equation for tagging?
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
                  for x:=1 to _isos do // another cycle for explicit
{$ENDIF}
                    begin

                    a:=' <';
                    a+=substr(drsyntax,'#',abbr)+tagabbr+clsname[k]+isoatom;
                    if (if2t) then                       // in case both educts are tagged
{$IFNDEF DBL_EXPL}
                       a+='i'+inttostr(e);               // impl. quad. nomenclature: G4110I12Ci1
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
                    af:=qae[e]/(qae[1]+qae[2]);

                    for j:=1 to _prod do     // cycling products
{$IFNDEF DBL_EXPL}
                        if (in_tsl(prod[j])) then        // filtering out all non-tagged species in implicit
{$ENDIF}
                           begin
                           if (a[length(a)-1]<>'=') then a+=' + '; // if right side is not empty
                           savstoi:=stoi[j];
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

                              // if # of atoms in product >= # atoms in educt:  constructing molecule
                              // if # of atoms in product < # atoms in educt: breaking to molecules
                              if not(pf=1.0) then
                                 begin
                                 if ((stoi[j]*(1.0-pf))<>1.0) then
                                    b+=formatfloat('0.0###########',stoi[j]*(1.0-pf)); // coeff. for a major isotopol. in minor reac
                                 if ((1.0-pf)>0) then
                                    b:='+'+b;                             // insert "+" in case of positive stoi coeff
                                 insert(' ',b,2);                         // insert space between stoi coeff and its sign
                                 b:=' '+b+' ';                            // stylish?
                                 b+=giveiso(prod[j],1);                   // major isotopologue
                                 stoi[j]:=stoi[j]*pf;                     // accounting minor transfer prob. in stoi
                                 end;
                              end;

                           if (stoi[j]<>1.0) then                           // placing <>1 stoi coefficient
                              a+=formatfloat('0.0###########',stoi[j])+' '; // by trial&error found: MECCA makes no difference in
                           a+=giveiso(prod[j],k);                           //   precision starting from 12 dig. after the comma

                           a+=b;

                           // reverting changes in stoichiom. coeff
                           stoi[j]:=savstoi;
                           end;

                    // checking if there are no produts on the right side (USE_PT off & destruction to nothing - quite possible)
                    b:=trim(a); if (b[length(b)]='=') then a:=a + 'Dummy ';
                           
{$IFDEF USE_PT}

 {$IFDEF DBL_EXPL}
                    // PTs in explicit case: all equations have the same PT - total production
                    if (pos(substr(ptsyntax,'#',abbr),eqnfile[l].line)=0) then
                       // adding production tracer
                       a+=' + '+substr(ptsyntax,'#',abbr);
 {$ENDIF}

 {$IFDEF USE_PT_KIE}
                    // add passive tracer to the the reaction which has kie
                    for j:=1 to _kie do
                        if (kie[j].imec) and (abbr=kie[j].abbr) then
                           begin
                           a+=' + '+substr(ptsyntax,'#',abbr+tagabbr+clsname[k]+isoatom);
  {$IFNDEF DBL_EXPL}
                           if (if2t) then a+='i'+inttostr(e);
  {$ELSE}
                           if (if2t) then a+='e'+inttostr(x);
  {$ENDIF}
                           break;
                           end;
 {$ENDIF}
{$ENDIF}

                    // in case educts are equal, avoiding "double" reaction for mecca
                    if ((if2t) and (educ[1]=educ[2]) and (e=2)) then
{$IFNDEF DBL_EXPL}
                       a+=' + Dummy';
{$ELSE}
                       a+=' + '+floattostr(x-1.0)+ ' Dummy';
{$ENDIF}

(*
                    // this is checked later by check_eqn_dupes
                    // applied to only MECCA2.0 reaction list! attention!     ??!!??
                    if (e=1) and ((abbr='G7402b') or (abbr='J4101b')) then
                       a+=' + Dummy ';
*)

                    // adding reaction rate
{$IFNDEF USE_DKRATE}
                    a+=' :'+phys;
{$ELSE}
                    a+=' :'+' {%*} '+substr(dkratesyntax,'#',abbr)+'; {&&}';
{$ENDIF}
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
                 // if products have no sources in .eqn source (source specification)
                 with src[eqs[eqnfile[l].eqno].nsrc] do
                      for e:=1 to _trans do                 // e cycles through source specification
                          if (trans[e]._dst>0) then         // avoiding sources without destination
                             begin

                             // destroying tagged educts (if any) without fractionation (supposed)
                             if (e=1) then   // this condition is to work only once, in the beginning
                                for k:=1 to _isos do
                                    for j:=1 to 2 do
                                        if in_tsl(educ[j]) then
                                           begin
                                           write(f,' <'+substr(drsyntax,'#',abbr)+tagabbr+clsname[k]+isoatom+'s'+inttostr(j)+'> ');
                                           if (educ[3-j]<>'') then
                                              write(f,educ[3-j]+' + ');
                                           write(f,giveiso(educ[j],k)+' = ');
                                           if (educ[3-j]<>'') then
                                              write(f,educ[3-j])
                                           else
                                               write(f,'Dummy');
                                           if ((educ[1]=educ[2]) and (j=2)) then
                                              write(f,' + Dummy');
{$IFNDEF USE_DKRATE}
                                           writeln(f,' :'+phys);
{$ELSE}
                                           writeln(f,' :'+' {%*} '+substr(dkratesyntax,'#',abbr)+'; {&&}');
{$ENDIF}
                                           end;

                             // q-ty of atoms in educts
                             qae[1]:=tsl[no_tsl(trans[e].src)].qatm;
                             qae[2]:=0;                // source specification is now only from one source

                             // then istopologues
                             for k:=1 to _isos do
                                 begin

                                 // new r-n abbr
                                 write(f,' <'+substr(drsyntax,'#',abbr)+tagabbr+clsname[k]+isoatom+'s'+trans[e].src+'> ');
                                 a:=educ[1];                    // 1st educt
                                 if (educ[2]<>'') then
                                    a+=' + '+educ[2];           // 2nd educt
{$IFNDEF DBL_EXPL}
                                 a+=' = '+a;                    // replicate "regulars" @ right side
{$ELSE}
                                 a+=' = ';                      // explicit doubling
{$ENDIF}

                                 for j:=1 to _prod do
{$IFNDEF DBL_EXPL}
                                     if (in_tsl(prod[j])) then
{$ENDIF}
                                        begin

                                        // should be accounted in current (e) source?
                                        if ( no_trans_dst(eqs[eqnfile[l].eqno].nsrc,e,prod[j])>0 ) then
                                           begin
                                           savstoi:=stoi[j];
                                           if (a[length(a)-1]<>'=') then a+=' + ';  // if right side is not empty

                                           // accounting the number of total source specification(s) for this species
                                           stoi[j]:=stoi[j]*trans[e].dst[no_trans_dst(eqs[eqnfile[l].eqno].nsrc,e,prod[j])].stoi;

                                           // accounting the transfer of the minor isotope atom
                                           // from educt molecule to current product molecule
                                           b:='';
                                           if (in_tsl(prod[j]) and (k<>1)) then        // if reaction is for the minor isotopologue transfer
                                              begin
                                              qap:=tsl[no_tsl(prod[j])].qatm;          // qty. of atoms in product
                                              pf:=qap/qae[1];                          // prob = A(prod)/A(educ), i.e. 1/3 for C3H6 -> CH3O2

                                              // if # of atoms in product >= # atoms in educt:  constructing molecule
                                              // if # of atoms in product < # atoms in educt: breaking to molecules
                                              if not(pf=1.0) then
                                                 begin
                                                 if ((stoi[j]*(1.0-pf))<>1.0) then
                                                    b+=formatfloat('0.0###########',stoi[j]*(1.0-pf)); // coeff. for a major isotopol. in minor reac
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

                                           // reverting changes in stoichiom. coeff
                                           stoi[j]:=savstoi;
                                           end;
                                         end;

{$IFDEF USE_PT}
 {$IFDEF DBL_EXPL}
                                 // PTs in explicit case
                                 if (pos(substr(ptsyntax,'#',abbr),eqnfile[l].line)=0) then
                                    // adding production tracer
                                    a+=' + '+substr(ptsyntax,'#',abbr);
 {$ENDIF}
 {$IFDEF USE_PT_KIE}
                                 // add passive tracer to the the reaction which has kie
                                 for j:=1 to _kie do
                                     if ((kie[j].imec) and (abbr=kie[j].abbr)) then
                                        begin
                                        a+=' + '+substr(ptsyntax,'#',abbr+tagabbr+clsname[k]+isoatom);
                                        break;
                                        end;
 {$ENDIF}
{$ENDIF}
                                 // adding reaction rate
{$IFNDEF USE_DKRATE}
                                 a+=' :'+phys;
{$ELSE}
                                 a+=' :'+' {%*} '+substr(dkratesyntax,'#',abbr)+'; {&&}';
{$ENDIF}
                                 // weighting reaction rate with corresponding isotopomer fraction
                                 insert('( ',a,pos('}',a)+2); // adding left brace
                                 insert(' )*'+'src_'+trans[e].src+'_f'+tagabbr+clsname[k]+isoatom,a,pos(';',a));

                                 // checking whether the reaction has kie
                                 for j:=1 to _kie do
                                     if (abbr=kie[j].abbr) then
                                        if (kie[j].isot=tsl[no_tsl(educ[e])].isos[k]) then
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
                         // istopologues
                         for k:=2 to _isos do
                           with iex[niex] do
                             begin

                             // new r-n abbr
                             a:=' <'+substr(drsyntax,'#',abbr)+tsl[exspec[e]].isos[k]+'> ';
                             a+=tsl[exspec[e]].isos[k]  +' + ';    // major
                             a+=tsl[exspec[3-e]].isos[1]+' = ';    // rare

                             // no-isotopomers approach: exchanging rare isotope atom (one) only
                             a+=tsl[exspec[e]].isos[1]+' + ';      // now rare
                             a+=tsl[exspec[3-e]].isos[k];          // now major
            
                             // rate
{$IFNDEF USE_DKRATE}
                             a+=' :'+phys;
{$ELSE}
                             a+=' :'+' {%*} '+substr(dkratesyntax,'#',abbr)+'; {&&}';
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

                             // calculating transfer probability (if rare sharing molec. has more than 1 isotope atom)
                             if (tsl[exspec[e]].qatm>1) then
                                insert('(1.0/'+inttostr(tsl[exspec[e]].qatm)+'.0)*',a,pos('}',a)+2);

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
          write(f,'<D0000T',isoatom,'> '+tagabbr+'T'+isoatom,' = ',
                                     tagabbr+clsname[1]+'T'+isoatom);
          for j:=2 to _isos do
              write(f,' + '+tagabbr+clsname[j]+'T'+isoatom);
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

       if (_kie>0) then                    // if there is KIE present
          begin
          writeln(f,'{------ [imdouble] - KIE process section for ',isoatom,' -------------------------------}');
          write(f,imcom_parse_kieproc);   // kie processing for doubling
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

procedure check_eqn_dupes(fname : string);
var f, o : textfile;
    e : array[1..max_eqs*max_isos] of record 
        eq : string;
        line, corr : integer;
        end;
    a : string;
    l, p, i, j : integer;
begin

write('check_eqn_dupes(',fname,'): ');

assign(f,fname);
reset(f);

p:=0; l:=0;
while not(eof(f)) do
      begin
      inc(l);
      readln(f,a);
      if imcom_is_eqn(a) then
         begin
         inc(p);
         e[p].eq:=imcom_ext4marks(a,'>',':');          // copier l'equation
         e[p].eq:=uppercase(delspace(trim(e[p].eq)));  // >JE+SUIS+UNE=EQUATION+0.5MAINTENANT:
         e[p].line:=l;
         e[p].corr:=0;
         end;
      end;
close(f);

// bubblesort comparison & correction
for i:=1 to p-1 do
    for j:=i+1 to p do
        if (e[j].eq=e[i].eq) then
           begin
           inc(e[j].corr);
           e[j].eq+=uppercase('+Dummy');
           end;

// apporter des modifications
assign(o,fname+'.$$$'); rewrite(o);
assign(f,fname); reset(f);

j:=1; // indice a e[]
l:=0; // indice a f
p:=0;
while not(eof(f)) do
      begin
      inc(l);
      readln(f,a);
      if (e[j].line=l) then
         begin
         
         if (e[j].corr>0) then
            begin
            insert('+ '+inttostr(e[j].corr)+' Dummy ',a,pos(':',a));
            inc(p);
            end;
            
         inc(j);
         end;
      writeln(o,a);
      end;
close(f);
close(o);
rename(o,fname);

if (p>0) then
   write(p,' identical equations found, corrected')
else
    write(' no identical equations found');
writeln;

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
             addspecs_intr+=':'+tagabbr+clsname[j]+spec+'>';
             inc(_addspecs_intr);
             end;
addspecs_intr+=':'+tagabbr+'T'+isoatom+'>';
for i:=1 to _isos do
    addspecs_intr+=':'+tagabbr+clsname[i]+'T'+isoatom+'>';
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
writeln(f,'{ configuration: ',tagabbr+isoatom,' }');
writeln(f,'{ intended # of species to add: ',_addspecs_intr,' }');
{$IFDEF USE_PT}
writeln(f,'{ intended # of PTs to add: ',_ptracs_intr,' }');
{$ENDIF}
writeln(f);
writeln(f,'{ [Gromov, MPI-C] =',datetimetostr(now),'= }');
writeln(f);

writeln(f,'{- new atoms -----------------------------------------------------------------}');
writeln(f);
writeln(f,'#ATOMS');
writeln(f);
for i:=1 to _isos do
    writeln(f,'  ',isoatom+clsname[i],'     { ',isomass[i],' mass element ',isoatom,' };');
writeln(f);

writeln(f,'{- doubled species -----------------------------------------------------------}');
writeln(f);

a:='';

(* this method is no longer used since the introduction of spc[] and work with species file
tmp:=addspecs_intr;
while (length(tmp)>0) do
      begin
      // one addspecs entry is like :ISOP>

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
             safeaddspec(isos[j],spc[nspc].icomp[j]+'; { '+spec+' '+clsname[j]+isoatom+' isotopologue }');
         writeln(f);
         
         end;

writeln(f,'{ totals }');
writeln(f);
safeaddspec(tagabbr+'T'+isoatom,'IGNORE; { total '+isoatom+' atoms count }');
for j:=1 to _isos do
    safeaddspec(tagabbr+clsname[j]+'T'+isoatom,'IGNORE; { total '+clsname[j]+isoatom+' atoms count }');
writeln(f);

{$IFDEF USE_PT}
writeln(f,'{- passive production tracers ------------------------------------------------}');
writeln(f);

for i:=1 to _eqs do
    with eqs[i] do
         if (itag) then
            safeaddspec(substr(ptsyntax,'#',abbr),'IGNORE; { '+abbr+' reaction passive production tracer }');
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

      writeln(f,'  ',substr(ptsyntax,'#',a),' = IGNORE; { ',a,' reaction production }');
      end; *)

 {$IFDEF USE_PT_KIE}
// in case of changes refer to the imcom_update_ptracs section in imcom.inc
for k:=1 to _kie do
    if (kie[k].imec) then       // if KIE exist in this configuration
       with kie[k] do
            for j:=1 to _isos do
 	        if not( in_tsl(eqs[kie[k].eqno].educ[1]) and in_tsl(eqs[kie[k].eqno].educ[2]) ) then    // not a quadrupl. reaction
		   safeaddspec(abbr+tagabbr+clsname[j]+isoatom,'IGNORE; { '+abbr+' reaction '+isot+' KIE production tracer }')
                else
  {$IFNDEF DBL_EXPL}
		    begin	    // in case both educts are tagged (quadrupled equation = quad. kie PTs)
		    safeaddspec(kie[k].abbr+tagabbr+clsname[j]+isoatom+'i1',
                                'IGNORE; { '+abbr+' reaction '+isot+' KIE production tracers }');
		    safeaddspec(kie[k].abbr+tagabbr+clsname[j]+isoatom+'i2',
                                'IGNORE;');
                    end;
  {$ELSE}
		    safeaddspec(kie[k].abbr+tagabbr+clsname[j]+isoatom+'e'+inttostr(1),
                                'IGNORE; { '+abbr+' reaction '+isot+' KIE production tracers }');
                    for i:=2 to _isos do  // in case of explicit doubling (_iso-replicated equations with KIE)
		        safeaddspec(kie[k].abbr+tagabbr+clsname[j]+isoatom+'e'+inttostr(i),'IGNORE;');
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

// main part
begin

write('produce_imdouble_code(',fname,'): ');

// filling replacements
imcom_update_reps;

imcom_check_files_exist([tname, kieproc]);

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
          if pos('{$CONF_LIST',a)>0 then
             writeln(f,imcom_make_configslist(imcom_ext4marks(a,'[%','%]')))
          else
          if pos('{$TRAC_DECL',a)>0 then
                 writeln(f,imcom_make_tracdecl(imcom_ext4marks(a,'[%','%]'),
                                               imcom_ext4marks(a,'(%','%)')))
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

if (paramcount<2) then
   begin
   writeln('>> MECCA kinetic meccanism doubling');
   writeln('usage: ./imdouble <spcfile> <eqnfile> <tagging configuration(s) list>');
   writeln('   ex:             zzz.spc   yyy.eqn   tagXX.cfg');
   halt;
   end;

writeln('[imdouble]');
writeln('=',datetimetostr(now),'=');
writebreak;

{$IFDEF DBL_EXPL}

SORRY, EXPLICIT VERSION NEEDS RE-FORMULATION AND RE-CODING NOW

if (paramcount>2) then
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

_conf:=paramcount-2;

for nconf:=1 to _conf do
    begin

    writeln;
    writeln('>> doubling ',nconf,' of ',_conf,' configuration(s)...');
    writeln;

    imcom_check_files_exist([paramstr(nconf+2),paramstr(1),paramstr(2)]);

    // read tagging config
    imcom_read_tag_config(paramstr(nconf+2));

    // read species ans equations files interpreting according to the loaded config
    if (nconf=1) then
       begin
       imcom_read_spc(paramstr(1));
       imcom_read_eqs(paramstr(2));
       end
    else
        begin
        imcom_read_spc(l_spcfname); // next configuration is based on previously created spc/eqn files
        imcom_read_eqs(l_eqnfname);
        end;
    l_eqnfname:=eqnfname;
    l_spcfname:=spcfname;
    
    // updating inter-conf. PTs list
    imcom_update_ptracs;

    // updating inter-conf. additional specs. list
    imdouble_update_addspecs;

    // double the meccanism
    doubleit(eqnfname,nconf);                   // creating a new equation file
    imdouble_produce_speciesfile(spcfname);  // creating an additional specs file
    for i:=1 to _form_conf do
        produce_imdouble_code(form_conf[i,1],form_conf[i,2]);

    // check for a possible duplicate reactions
    check_eqn_dupes(eqnfname);

    conf[nconf]:=tagname;
    
    imcom_make_addtracdef('gas.tex',tracdef);
    end;

writeln;
if (_form_intr>0) then writeln('inter-configuration formers:');
for i:=1 to _form_intr do
    produce_imdouble_code(form_intr[i,1],form_intr[i,2]);

writeln;
writeln('[imdouble]: done');
writeln;

end.
