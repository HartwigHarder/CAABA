// en FPC >= 2.2.0

// = imdouble.pas ====================================================
//
// im-tag main source module
// requires: all is done through imcom.inc
//
// Gromov, MPIC, 2007-2008
// ===================================================================

program imtag;

// - conditional defines -------------------------------------------

{$DEFINE TAG}         // for imcom: compiling imtag

{$DEFINE xUSE_RN}     // defines whether to use the reaction
                      // sequential number in the reactions list
                      // instead of passive tracers names

{$DEFINE USE_PT}      // using passive tracers, should be always ON

{$DEFINE USE_KRSIND}  // defines whether to output the list of
                      // KIE-related specs indices (for correction)

{$DEFINE xCRT_INTEQN} // defines whether to create an additional
                      // equation file with isotopologues
                      // interchange reactions

{$DEFINE MAKE_DOTS}   // defines whether to create .dot files for
                      // graphvis showing species interrelation

// -----------------------------------------------------------------

//  linking common utils/types include file for im-double/tag
{$I imcom.inc}

// -----------------------------------------------------------------

// - TAGGING PART --------------------------------------------------

// begin-chain species
var  bcs : array[1..max_tsl] of string;
    _bcs : word;

procedure make_bcs;
var s, i, j : word;
    f : boolean;
begin
writeln('begin-chain (all) species:');
_bcs:=0;
for s:=1 to _tsl do
    begin
    f:=false;
    for i:=1 to _eqs do
        with eqs[i] do
             for j:=1 to _prod do
                 if (prod[j]=tsl[s].spec) then
                    f:=true;
    if not(f) then
       begin
       inc(_bcs);
       bcs[_bcs]:=tsl[s].spec;
       end;
    end;

writeln('#',_bcs,' species: ');
for i:=1 to _bcs do
    write(bcs[i],' ');
writeln;
writebreak;
end;

function in_bcs(spec : string) : boolean;
var i : word;
begin
in_bcs:=false;
for i:=1 to _bcs do
    if (spec=bcs[i]) then
       begin in_bcs:=true; break; end;
end;

// -----------------------------------------------------------------

const max_reac = 300;  // maximum amount of reactions containing
                       // one selected species (increase if necessary)

type ttagset = array[1..max_tsl] of record
     spec : nstr;                             // species name
    _prod : word;                             // & # of reactions
     prod : array[1..max_reac] of word;       // reaction #
     prec : array[1..max_reac] of string;     // who produced
     stoi : array[1..max_reac] of real;       // & corr. stoichiometric coeff
     _f2t : array[1..max_reac] of boolean;    // both educts are tagged?

    _sink : word;
     sink : array[1..max_reac] of word;
     frac : array[1..max_reac] of real;       // fractionation coeff (???)

     ifix : boolean;                          // is fixed?
     end;

// - tagit ---------------------------------------------------------------------
// investigate the tagging information of the
// current mechanism and allowed tagged species
procedure tagit(var d : ttagset);

var i, j : integer;
    e, r, s, p  : integer;

begin

// make begin-chain species list
make_bcs;

writeln('taginfo:');

for s:=1 to _utsl do         // cycle through allowed transient species
    with d[s] do

    begin

    // initialize record for each
    spec:=tsl[utsl[s]].spec;
   _prod:=0;
    fillchar(prod,sizeof(prod),0);
    fillchar(prec,sizeof(prec),0);
    fillchar(frac,sizeof(frac),0);
    fillchar(_f2t,sizeof(_f2t),0);

   _sink:=0;
    fillchar(sink,sizeof(sink),0);

    ifix:=tsl[s].ifix;

//    if (ifix) then continue;             // skipping the fixed species?

    for r:=1 to _eqs do // cycle through reactions list
        if (eqs[r].itag) then          // checking "dedicated for tagging" flag (additional)
           begin

           // so far skipping exchange reactions
           if (eqs[r].iiex) then continue;
   
           // inspecting sink
           // it doesn't matter if the the interesting species appears
           // twice as both educts at the left-side                    (???)
           if ((eqs[r].educ[1]=spec) or (eqs[r].educ[2]=spec)) then
              begin
              inc(_sink);
              sink[_sink]:=r;
              end;
   
           if (eqs[r].isrc) then                  // source-specified reaction, treating separately
              for e:=1 to src[eqs[r].nsrc]._trans do
                  for p:=1 to src[eqs[r].nsrc].trans[e]._dst do
                      if (src[eqs[r].nsrc].trans[e].dst[p].spec=spec) then
                         begin
                         inc(_prod);
                         prod[_prod]:=r;
                         stoi[_prod]:=src[eqs[r].nsrc].trans[e].dst[p].stoi;
                         prec[_prod]:=src[eqs[r].nsrc].trans[e].src;
                         _f2t[_prod]:=false;
                         end
                      else 
           else
               for p:=1 to eqs[r]._prod do        // regular r-n, equal transfer probability
                   if (eqs[r].prod[p]=spec) then
                      for e:=1 to 2 do            // cycle through educts to determine whether one is tagged (or both!)
                          if (in_tsl(eqs[r].educ[e])) then
                             begin
                             // avoid double account if both educts are same
                             if ((e=2) and (eqs[r].educ[1]=eqs[r].educ[2])) then
                                begin
                                _f2t[_prod]:=false;
                                continue;
                                end;
       
                             inc(_prod);
                             prod[_prod]:=r;
                             stoi[_prod]:=eqs[r].stoi[p];
                             prec[_prod]:=eqs[r].educ[e];
                             if (in_tsl(eqs[r].educ[3-e])) then
                                _f2t[_prod]:=true;  // if (2nd for 1st) or (1st for 2nd)
                                                    // in tagged species list - then the flag
                                                    // "isotope comes from 2 tagged educts" is set
                             end;
   
           end;
   
    writeln('-',spec,'-------------------------------------------------------------- ');

    write('S (',_sink,'): ');
    for i:=1 to _sink do
        begin
        for j:=1 to _kie do
            if (kie[j].imec) then
               if (eqs[sink[i]].abbr=kie[j].abbr) then
                  write('KIE[',kie[j].isot,']~');
        write(eqs[sink[i]].abbr,' ');
        end;
    writeln;

    write('P (',_prod,'): ');
    for i:=1 to _prod do
        begin

        if (eqs[prod[i]].isrc) then
           write('SUB:');

        for j:=1 to _kie do
            if (kie[j].imec) then
               if (eqs[prod[i]].abbr=kie[j].abbr) then
                  write('KIE[',kie[j].isot,']:');

        if not(stoi[i]=1.0) then
           write(floattostr(stoi[i]),'*');

        write(eqs[prod[i]].abbr);

        if _f2t[i] then
           write('>2S<');

        write('[',prec[i],']  ');
        end;
    writeln;

    end;

writeln;
writeln('tagit(',paramstr(1),',',paramstr(2),'): done');

end;


// - CODE-CREATING PART --------------------------------------------------------

var flow_conf_calc : ansistring; // calculation code with templates
    flow_conf_dirs : ansistring; // number of direction blocks to calculate (i.e. counting CH4_sink, CH4_CH3O2, etc.)
    flow_conf_ecnt : integer;    // directions list
    flow_conf_ukie : ansistring; // kie-update code for each configuration

var flow_intr_calc : ansistring;  // same as above but for all configurations
    flow_intr_dirs : ansistring;
    flow_intr_ecnt : integer;

var jac_conf_calc : ansistring;   // jacobian computation code for configuration
    jac_conf_ecnt : integer;      // no. of nonzero elements

type // spec. jacobian matrix for simplified linear computation -- using heap
    tjacelm = array[1..max_tsl,1..max_tsl] of string[200];
var jacelm : ^tjacelm;

// prepare_flow_calc -----------------------------------------------------------
// code for species2species molecules "flow" and
// related elements (jacobian, coeff. matrix) calculation
procedure prepare_flow_jac_calc(d : ttagset);
//procedure write_isot_flow;
var s, r, j : word;
    a, b, c, done, p : ansistring;
{$IFDEF CRT_INTEQN}
    e : textfile;
{$ENDIF}
begin

{$IFDEF CRT_INTEQN}
assign(e,cmodel+tagname+'.int.eqn');
rewrite(e);
{$ENDIF}

getmem(jacelm,sizeof(tjacelm));
fillchar(jacelm^,sizeof(tjacelm),0);

// calculation code with templates
flow_conf_calc:='  ! ----- '+tagname+': flow(FROM_TO[sink]) = SUM( PTs(FROM->TO[sink]) ) -----'+_LF;
flow_conf_ecnt:=0;    // number of direction blocks to calculate (i.e. counting CH4_sink, CH4_CH3O2, etc.)
flow_conf_dirs:='';   // directions list                                                                  
flow_conf_ukie:='';   // kie-update code

jac_conf_calc:='';
jac_conf_ecnt:=0;

for s:=1 to _utsl do
    with d[s] do
         begin

         p:=_LF;
         p+='  ! ----- '+tagabbr+isoatom+': '+spec+' -----';
         if (ifix) then
            p+=' WARN: indicated as fixed species';
         p+=_LF;

         // inspecting sink
         if (_sink>0) then

            begin
            a:=p+'    flow('+spec+'_sink) = ( ';
            for r:=1 to _sink do
                begin

                // in case compound reacts with itself, 2 molecules are consumed
                if (eqs[sink[r]].educ[1]=eqs[sink[r]].educ[2]) then
                   a:=a + '2.00_dp * ';
                // storing reaction name and internal number
                a+='{'+eqs[sink[r]].abbr+'}';
                a+=' + ';                   // for a next sink
                end;
            setlength(a,length(a)-2);       // cut last plus "+"
            a+=')';

            // jacobian
            jacelm^[no_tsl(spec),no_tsl(spec)]:=jacelm^[no_tsl(spec),no_tsl(spec)]+
                                               '-flow('+spec+'_sink)';

{$IFDEF CRT_INTEQN}
            for j:=1 to _isos do
                writeln(e,'<'+tagabbr+clsname[j]+isoatom+formatfloat('000',flow_conf_ecnt)+'> ',
                          tagabbr+clsname[j]+spec,' = Dummy : {%StTrG} ',
                          'F'+clsname[j]+isoatom+'(ind_t'+spec+')*flow('+spec+'_sink); {&&}');
{$ENDIF}

            // flow direction
            c:='/'+spec+'>'+'sink'+':';
            // configuration flow code
            flow_conf_calc+=a+_LF; //wrap(a,8,8);
            flow_conf_dirs+=c;
            inc(flow_conf_ecnt);
            // inter-configuration flow code
            if (pos(c,flow_intr_dirs)=0) then
               begin
               flow_intr_calc+=a+_LF;
               flow_intr_dirs+=c;
               inc(flow_intr_ecnt);
               end;

            end;

         // inspecting production
         if (_prod>0) then
            begin

            done:='';

            // collecting reactions for the same species into the brackets
            // collecting quadrupled reactions in 0.5_dp * ( ) brackets
            for r:=1 to _prod do
                if (not(pos('>'+prec[r]+'<',done)>0)) then
                   begin

                   a:=''; // regular components part
                   b:=''; // this is 0.5_dp * ( ... ) part for quadruples
                   c:=''; // temp
                   for j:=1 to _prod do
                       if (prec[j]=prec[r]) then
                          begin

                          c:='';
                          if (stoi[j]<>1.0) then
                             c:=formatfloat('##0.0###############',stoi[j]) + ' * ';     {floattostr}

                          // storing reaction name and internal number
                          c+='{'+eqs[prod[j]].abbr+'}';

                          // if both educts are tagged, then accounting
                          // only half of production (quadrupled case in doubling) -> b
                          if (_f2t[j]) then
                             b:=b+c+' + '
                          else           // else, regular ->a
                              a:=a+c+' + ';
                          end;

                   if (length(a)>0) then
                      begin
                      setlength(a,length(a)-2);       // remove last plus "+"
                      a:='( '+a+')';                  // regular components
                      end;
                   if (length(b)>0) then
                      begin
                      setlength(b,length(b)-2);       // remove last plus "+"
                      if (length(a)>0) then a:=a+' + ';
                      a:=a+'0.5_dp * ( '+b+')';       // quadrupled components
                      end;

                   a:='    flow('+prec[r]+'_'+spec+') = '+a;
                   if (_sink=0) then a:=p+a;

                   done+='>'+prec[r]+'<';

                   // jacobian
                   jacelm^[no_tsl(spec),no_tsl(prec[r])]:=jacelm^[no_tsl(spec),no_tsl(prec[r])]+
                         '+flow('+prec[r]+'_'+spec+')';

{$IFDEF CRT_INTEQN}
                   for j:=1 to _isos do
                       writeln(e,'<'+tagabbr+clsname[j]+isoatom+formatfloat('000',flow_conf_ecnt)+'> ',
                                  tagabbr+clsname[j]+prec[r]+' = ',
                                  tagabbr+clsname[j]+spec+' : {%StTrG} ',
                                  'F'+clsname[j]+isoatom+'(ind_t'+prec[r]+')*flow('+prec[r]+'_'+spec+'); {&&}');
{$ENDIF}

                   // flow direction
                   c:='/'+prec[r]+'>'+spec+':';
                   // inter-configuration flow code
                   flow_conf_calc+=a+_LF; //wrap(a,8,8);
                   flow_conf_dirs+=c;
                   inc(flow_conf_ecnt);
                   // inter-configuration flow code
                   if (pos(c,flow_intr_dirs)=0) then
                      begin
                      flow_intr_calc+=a+_LF;
                      flow_intr_dirs+=c;
                      inc(flow_intr_ecnt);
                      end;
                
                   end;

            end;

         end;

// making KIE corrections calculation former
flow_conf_ukie:='  ! ----- '+tagname+': flow(KIE_REAC_ISOT) corrections calculation -----'+_LF+_LF;
for j:=1 to _kie do
    if (kie[j].imec) then
       begin
           
       c:=kie[j].abbr+'_'+kie[j].isot;
       flow_conf_ukie+='    flow(KIE_'+c+') = '+
                       '{'+kie[j].abbr+'} * (1.0_dp - 1.0_dp'+kie[j].expr+')' +_LF;
                  
       flow_conf_dirs+='/'+'KIE'+'>'+c+':';            // KIE corrections are included
       flow_intr_dirs+='/'+'KIE'+'>'+c+':';            // to the directions
                  
       inc(flow_conf_ecnt);
       inc(flow_intr_ecnt);
                  
       end;
           
// forming items of the jac matrix
jac_conf_ecnt:=0;
for s:=1 to _utsl do        // source
    for r:=1 to _utsl do    // receiver
        if (length(jacelm^[utsl[r],utsl[s]])>0) then
           begin
           // >< markers for receiver, () for sink spec., line after ':' = flow calc.
           jac_conf_calc+='/'+tsl[utsl[s]].spec+'>'+tsl[utsl[r]].spec+'='+
                   jacelm^[utsl[r],utsl[s]]+':';   
           inc(jac_conf_ecnt);
           end;

freemem(jacelm,sizeof(tjacelm));

writeln('prepare_flow: done');
writeln('  flow_calc: ',flow_conf_ecnt,' entries (all flow: ',flow_intr_ecnt,')');  // some info
writeln('  jacobian: ',jac_conf_ecnt,' entries');

{$IFDEF CRT_INTEQN}
close(e);
{$ENDIF}

end;

//
//
function imtag_make_flow_calc(flowstr, ptvalstr : string) : ansistring;
var smp, out : ansistring;
    i : integer;
begin

out:=flowstr;
if (ptvalstr='') then ptvalstr:='C(ind_'+ptsyntax+')';

for i:=1 to _eqs do
    begin
    smp:=substr(ptvalstr,'#',eqs[i].abbr);
    smp:=substr(smp,'$',inttostr(eqs[i].ntag));
       
    out:=ansireplacetext(out,'{'+eqs[i].abbr+'}',smp);
    end;
    
imtag_make_flow_calc:=wrap(out,6,6);

end;

//
//
function imtag_make_flow_dirs(dirstr, sample : string) : ansistring;
var tmp, smp, sink, recv, out : ansistring;
    dirno : integer;
begin

out:='';
tmp:=dirstr;
dirno:=1;

while (length(tmp)>0) do
      begin
      
      // one flow_dirs template entry:  /sink>recv:
      
      smp:=copy2symbdel(tmp,'/');   // rem '/'
      sink:=copy2symbdel(tmp,'>');  // copy to '>'
      recv:=copy2symbdel(tmp,':');  // copy to ':'

      smp:=substr(sample,'#',sink+'_'+recv);
      smp:=substr(smp,'$',inttostr(dirno));
      
      out+=smp+',';
      inc(dirno);
      end;

setlength(out,length(out)-1);  // cut last comma
out:=trim(out);                // cut leading spaces

imtag_make_flow_dirs:=out;

end;


// d : tagging setup; tname, fname : former (template) and output filenames
procedure imtag_produce_code(d : ttagset; tname, fname : string);

var f : text;  // output file handle (all procs write to f)

// write_isot_derivs -----------------------------------------------------------
// code for selected isotopologue derivatives calculation
// -> now duped more flexible with flow() and jac, so may be useless
procedure write_isot_derivs(isot, _dest, _via, _frac, _step : string; last : boolean);

var s, r, j, k : word;
    a, p, done : ansistring;

begin

// default exchange variables names
if (_dest='') then _dest:=isot+'(#)';
if (_via='') then _via:=isot+'(#)';
if (_frac='') then _frac:='F'+isot+'(#)';
if (_step='') then _step:='delta';

_step:=_step + ' * ';
 _via:=_via + ' + ';

// "cancellable" variables
if (pos('!',_step)>0) then _step:='';
if (pos('!',_via)>0) then _via:='';

writeln(f,'    ! ===== ',tagname,': ',_dest,' <- ',_via,_step,'( ',_frac,'*flow() + ... ) =====');

for s:=1 to _utsl do
    with d[s] do
         begin

         writeln(f);

         write(f,'      ! ----- ',spec,' -----');
         if (ifix) then
            write(f,'  WARN: indicated as fixed species');
         writeln(f);

         // making substitutions for exchange variable names
         write(f,'      ',substr(_dest,'#',spec),
                    ' = ',substr(_via,'#',spec),
                          substr(_step,'#',spec));
         writeln(f,'( &');

         a:='';

         // sink
         if (_sink>0) then
            begin
            a:='        - '+format('%18s',[substr(_frac,'#',spec)])+' * ( flow('+spec+'_sink) )';

            // checking if there is KIE in sink
            for r:=1 to _sink do
                for k:=1 to _kie do
                    if (eqs[sink[r]].abbr=kie[k].abbr) then
                       if (isot=kie[k].isot) then
                          begin
{$IFDEF USE_RN}
                          p:='AR('+inttostr(eqs[sink[r]].ntag)+')';
{$ELSE}
                          p:='C(ind_'+substr(ptsyntax,'#',eqs[sink[r]].abbr)+')';
{$ENDIF}
                          p:=p+kie[k].expr+' - '+p;
                          // in case compound reacts with itself, 2 molecules are consumed
                          if (eqs[sink[r]].educ[1]=eqs[sink[r]].educ[2]) then
                             p:='2.0_dp * ( '+p+' )';

                          p:='+ '+p+' ';
                          // subtracting regular value and adding KIE-modified
                          insert(p,a,length(a));
                          end;

            if (_prod>0) then a:=a+'&';
            end;

         // production
         if (_prod>0) then
            begin

            done:='';

            // collecting reactions for the same species into the brackets
            for r:=1 to _prod do
                if (not(pos('>'+prec[r]+'<',done)>0)) then
                   begin
                   if (r>1) then      // LF for each new string
                      a:=a + '&';

                   a:=a + '        + '+
                      format('%18s',[substr(_frac,'#',prec[r])])+' * ( flow('+prec[r]+'_'+spec+') )';

                   // checking whether the reaction has kie
                   for j:=1 to _prod do
                       if (prec[j]=prec[r]) then
                          for k:=1 to _kie do
                              if (eqs[prod[j]].abbr=kie[k].abbr) then
                                 if (isot=kie[k].isot) then
                                    begin
{$IFDEF USE_RN}
                                    p:='AR('+inttostr(eqs[prod[j]].ntag)+')';
{$ELSE}
                                    p:='C(ind_'+substr(ptsyntax,'#',eqs[prod[j]].abbr)+')';
{$ENDIF}
                                    p:=p+kie[k].expr+' - '+p;

                                    // in case the stoichiometric coefficient is != 1
                                    // additional braces for numerical reasons
                                    if (stoi[j]<>1.0) then
                                       p:=formatfloat('##0.0###############',stoi[j])+' * ( '+p+' )';  {floattostr}

                                    // in case of quadrupled equation
                                    if (_f2t[j]) then
                                       p:='0.5_dp * ( '+p+' )';

                                    // subtracting regular value and adding KIE-modified
                                    p:='+ '+p+' ';
                                    insert(p,a,length(a));

                                    end;

                   done+='>'+prec[r]+'<';
                   end;

            end;

         write(f,wrap(a,33,8));

         writeln(f, ' )');        // delta brace

         end;
end;

//
//
function imtag_make_jac_calc(sample, kieisot : string) : ansistring;
var tmp, smp, out, a : ansistring;
    recv, sink, expr : string;
    i, j, k : integer;
begin

out:='';
tmp:=jac_conf_calc;

while (length(tmp)>0) do
      begin
      
      // one jacobian template entry:  /sink>recv=expr:
      
      smp:=copy2symbdel(tmp,'/');   // rem '/'
      sink:=copy2symbdel(tmp,'>');  // copy to '>'
      recv:=copy2symbdel(tmp,'=');  // copy to '='
      expr:=copy2symbdel(tmp,':');  // copy to ':'

      if (kieisot<>'') then
         begin
         // having sink and recv, searching for corresp. reactions with KIE
         // pushing changes to expr
         a:='';
         for k:=1 to _kie do
//           if ((kie[k].imec) and (kie[k].isot=kieisot)) then
             if ((kie[k].imec) and (pos(kieisot,kie[k].isot)>0)) then      //SO FAR ???
                with eqs[kie[k].eqno] do
                     if (educ[1]=sink) or (educ[2]=sink) then
                        if (recv=sink) then // diagonal element of the jacobian
                           begin
                           a:='flow(KIE_'+kie[k].abbr+'_'+kie[k].isot+')';
                           if (educ[1]=educ[2]) then
                              a:='2.0_dp*'+a;  // in case if compound reacts with itself
                           a:='+'+a;           // adding in case of sink!
                           end
                        else                // non-diagonal element
                            for i:=1 to _prod do
                                if (prod[i]=recv) then
                                   begin
                                   a:='flow(KIE_'+kie[k].abbr+'_'+kie[k].isot+')';
                                   if (stoi[i]<>1.0) then
                                      a:=floattostr(stoi[i])+'_dp*'+a;  // in case if compound has a stoichiom. coeff.
                                   a:='-'+a;   // subtracting in case of prod!
                                   end
             
         end;
        
      if ((kieisot<>'') and (a='')) then continue
                                    else expr+=a;
                  
      smp:=substr(sample,'~',expr);
      smp:=substr(smp,'#',sink);
      smp:=substr(smp,'$',recv);

      out+=smp+_LF;
      end;

imtag_make_jac_calc:=out;

end;


// imtag_fudgecode ---------------------------------------------------
// write "fudging" code to tune to the doubled mechanism
procedure imtag_write_fudgecode(sample : ansistring);
var i, j : word;
    tmp : ansistring;
begin

for i:=1 to _utsl do
    begin
    tmp:=substr(sample,'#',d[i].spec);
    writeln(f,tmp);
    end;
writeln(f);

end;


// main proc ----------------------------------------------------------

var t, finc : text;  // template file
    a, u, p, s : ansistring;
    i : integer;

// main part
begin

write('imtag_produce_code(',fname,'): ');

// checking necessary input files existence
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
              if pos('{$TAG_INFO}',a)>0 then
                 begin
                 writeln(f,'! source mech. equations: ',paramstr(1),' (',_eqs,' reactions)');
                 writeln(f,'!    tagged species list: ',paramstr(2),' (',_utsl,' of ',_tsl,' given species are tagged)');
//                 writeln(f,'! ',eqnfile[1].line);
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
          if pos('{$TAG_SPECS}',a)>0 then
             writeln(f,imcom_make_tagspecs_list(imcom_ext4marks(a,'[%','%]')))
          else
          if pos('{$x0}',a)>0 then
             writeln(f,imcom_make_x0(imcom_ext4marks(a,'[%','%]'),
                                     imcom_ext4marks(a,'(%','%)')))
          else
          if pos('{$FUDGE}',a)>0 then
             imtag_write_fudgecode(imcom_ext4marks(a,'[%','%]'))
          else
          if pos('{$PTII_DECL',a)>0 then
             writeln(f,imcom_make_ptracs(true))
          else
          if pos('{$RESET_PTs}',a)>0 then
             write(f,imcom_make_resetPTs(imcom_ext4marks(a,'[%','%]'),
                                         imcom_ext4marks(a,'(%','%)')))
          else
          if pos('{$DERIVS',a)>0 then
             begin
             for i:=1 to _isos do
                 if (pos(tagabbr+clsname[i]+isoatom,a)>0) then
                    write_isot_derivs(tagabbr+clsname[i]+isoatom,
                                      imcom_ext4marks(a,'[%','%]'),
                                      imcom_ext4marks(a,'(%','%)'),
                                      imcom_ext4marks(a,'<%','%>'),
                                      imcom_ext4marks(a,'{%','%}'),
                                      (pos('last',a)>0));
             end
          else
          if pos('{$FLOW_CALC}',a)>0 then
             if pos('INT',uppercase(a))>0 then   // inter-configuration
                writeln(f,imtag_make_flow_calc(flow_intr_calc,
                                               imcom_ext4marks(a,'[%','%]')))
             else
                 writeln(f,imtag_make_flow_calc(flow_conf_calc,
                                                imcom_ext4marks(a,'[%','%]')))
          else
          if pos('{$FLOW_DIRS}',a)>0 then
             if pos('INT',uppercase(a))>0 then   // inter-configuration
                writeln(f,wrap('    '+imtag_make_flow_dirs(flow_intr_dirs,
                                                           imcom_ext4marks(a,'[%','%]')),4,4))
             else
                 writeln(f,wrap('    '+imtag_make_flow_dirs(flow_conf_dirs,
                                                            imcom_ext4marks(a,'[%','%]')),4,4))
          else      
          if pos('{$JAC_CALC}',a)>0 then
             writeln(f,imtag_make_jac_calc(imcom_ext4marks(a,'[%','%]'),''))
          else
          if pos('{$JAC_KIE',a)>0 then
             for i:=1 to _isos do
                 if (pos(tagabbr+clsname[i]+isoatom,a)>0) then
                    writeln(f,imtag_make_jac_calc(imcom_ext4marks(a,'[%','%]'),
                                                  tagabbr+clsname[i]+isoatom))
                 else
          else
          if (pos('{$KIE_CALC}',a)>0) then
             writeln(f,imtag_make_flow_calc(flow_conf_ukie,
                                            imcom_ext4marks(a,'[%','%]')))
          else
          if (pos('{$KIE_PROC}',a)>0) then
             write(f,imcom_parse_kieproc)
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

writeln('done');

end;

// -----------------------------------------------------------------

// additional species list
procedure imtag_produce_spcPTs_file(fname : string);

var f : textfile;
    i : integer;
    declspecs, tmp, a : ansistring;

// add species checking for existing in the present species list
procedure safeaddspec(name, data : string);
begin
if (in_spc(name)) then declspecs+=name+' '
                  else writeln(f,'  ',name,' = ',data);
end;

begin

write('produce_spcPTs_file(',fname,'): ');

assign(f,fname);
rewrite(f);

// filling in previous species file
for i:=1 to _spcfile do
    writeln(f,spcfile[i].line);

writeln(f);
writeln(f,'{-------------- [imtag] ------------------------------------------------------}');
writeln(f,'{ [Gromov, MPI-C] =', datetimetostr(now),'= }');
writeln(f);
writeln(f,'{ additional species list for tagged mechanism based on: ',paramstr(1),' }');
writeln(f,'{ configuration: ',tagabbr+isoatom,' }');
{$IFDEF USE_PT}
writeln(f,'{ # of added passive tracers: ',_ptracs_intr,' }');
{$ELSE}
writeln(f,'{ running imtag version was compiled with USE_PT switched off }');
writeln(f,'{ passive tracers were NOT created                            }');
{$ENDIF}
writeln(f);
writeln(f,'{ [Gromov, MPI-C] =',datetimetostr(now),'= }');
writeln(f);

{$IFDEF USE_PT}
writeln(f,'{- passive production tracers ------------------------------------------------}');
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

for i:=1 to _eqs do
    with eqs[i] do
         if (itag) then
            safeaddspec(substr(ptsyntax,'#',abbr),'IGNORE; { '+abbr+' reaction passive production tracer }');

writeln(f);
{$ENDIF}

writeln(f,'{-------------- [imtag] - end ------------------------------------------------}');
writeln(f);

close(f);

if (declspecs<>'') then write('(warning, declined as already existing species: ',declspecs,') ');

writeln('done');

end;

// -----------------------------------------------------------------

// original equation file with added passive tracers
procedure imtag_produce_eqnPTs_file(fname : string);

var f : textfile;
    i, j : word;
    a, p : ansistring;

begin

write('produce_eqnPTs_file(',fname,'): ');

assign(f,fname);
rewrite(f);

// some info
writeln(f,'#INLINE F90_GLOBAL');
writeln(f,'! -------------------------------------------------------------------------');
writeln(f,'! Current mechanism (',paramstr(1),') is isotopically tagged by [imtag]');
  write(f,'! Atom: ',isoatom,', ',_isos,' isotopologues of masses ');
for i:=1 to _isos do
    write(f,clsname[i],' ');
writeln(f,'}');
writeln(f,'! # of tagged / added species: ',_utsl,' / ',(_utsl+1)*_isos+1);
writeln(f,'! # of reactions in the selected mechanism: ',_eqs);
writeln(f,'! # of tagged reactions / added passive tracers: ',nooftagreac,'(',_src,' subs)');
writeln(f,'! [Gromov, MPI-C] =', datetimetostr(now),'=');
writeln(f,'! --v-- further comes modified mechanism ----------------------------------');
writeln(f,'#ENDINLINE');
writeln(f);

for i:=1 to _eqnfile do
    if eqnfile[i].iseq then
       with eqs[eqnfile[i].eqno] do
            // ok, equation for tagging?
            if (eqs[eqnfile[i].eqno].itag) then
               begin

               // first, original eq with a production tracer added
               a:='<'+abbr+'> ';

               // educts
               a:=a+educ[1];
               if (educ[2]<>'') then a:=a+' + '+educ[2];
               a:=a+' = ';

               // products
               p:='';
               for j:=1 to _prod do
                   begin
                   if stoi[j]<>1.0 then
                      a:=a+floattostr(stoi[j])+' ';
                   a:=a+prod[j]+' + ';
                   p:=p+'>'+prod[j]+'<';
                   end;

               // checking if a PT is not already in the products list
               if (pos('>'+substr(ptsyntax,'#',abbr)+'<',p)=0) then
                  // adding production tracer
                  a:=a+substr(ptsyntax,'#',abbr)+' '
               else
                   // cutting last "+"
                   setlength(a,length(a)-2);

               // rest of equation (rate & comm.)
               writeln(f,a+' :'+phys);

               end
            else
                writeln(f,eqnfile[i].line)
  else
      writeln(f,eqnfile[i].line);

writeln(f);

close(f);

writeln('done');


{$IFDEF CRT_INTEQN}
assign(f,'messy_mecca_'+tagname+'.int.spc');
rewrite(f);
for i:=1 to _utsl do
    with tsl[utsl[i]] do
         for j:=1 to _isos do
             writeln(f,'  '+tagabbr+clsname[j]+spec+' = ',
                       inttostr(qatm)+isoatom+';');
close(f);
{$ENDIF}

end;

{$IFDEF MAKE_DOTS}

{$I imdot.inc}

{$ENDIF}

// main

var data : ttagset;
    nconf, i : integer;
    l_eqnfname, l_spcfname : string;   // eqn&spc filenames from the last processed configuration

begin

if (paramcount<2) then
   begin
   writeln('>> MECCA kinetic meccanism tagging');
   writeln('usage: imtag.exe <eqnfile> <tagging configurations list> ');
   writeln('   ex:            gas.eqn   tagc.ini            ');
   halt;
   end;

writeln('[imtag]');
writeln('=',datetimetostr(now),'=');
writebreak;

// inter-conf. initialization
imcom_init;
flow_intr_calc:='';
flow_intr_dirs:='';
flow_intr_ecnt:=0;

_conf:=paramcount-2;

for nconf:=1 to _conf do
    begin

    writeln;
    writeln('>> tagging ',nconf,' of ',_conf,' configuration(s)...');
    writeln;

    imcom_check_files_exist([paramstr(nconf+1),paramstr(1)]);

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

    // initialize and
    fillchar(data,sizeof(data),0);
    //                get tagging information
    tagit(data);

    // updating inter-conf. PTs list
    imcom_update_ptracs;

    writebreak;

    // creating new equation & additional specs file
    imtag_produce_eqnPTs_file(eqnfname);    // equation file with the passive tracers added
    imtag_produce_spcPTs_file(spcfname);    // additional species list

    // prepare the molecules flow calculation code for current configuration
    prepare_flow_jac_calc(data);
    // update template replacements
    imcom_update_reps;
    imcom_reps[20,2]:=inttostr(flow_conf_ecnt); // <-- # of curr. conf. flow dirs.

    // produce configuration F90 code from formers (templates)
    for i:=1 to _form_conf do
        imtag_produce_code(data,form_conf[i,1],form_conf[i,2]);

    // tracer definition file
    imcom_make_addtracdef('gas.tex',tracdef);

{$IFDEF MAKE_DOTS}
    // produce configuration dot files for graphviz
    //   species names should be sparated with spaces
    if (isoatom='C') then
       imdot_produce_dot_files(data,cmodel+'_'+tagname,'CH4 ISOP','CO')
    else
        if (isoatom='O') then
           imdot_produce_dot_files(data,cmodel+'_'+tagname,'OH O3','CO')
        else
            imdot_produce_dot_files(data,cmodel+'_'+tagname,'','');
{$ENDIF}

    writebreak;

    conf[nconf]:=tagname;
    end;

// inter-configuration part

// update template replacements
imcom_update_reps;
imcom_reps[imtag_QCFLDIR,2]:='-1';                     // <-- # of curr. conf. flow dirs.
imcom_reps[imtag_QIFLDIR,2]:=inttostr(flow_intr_ecnt); // <-- # of all conf. flow dirs.

writeln;
if (_form_intr>0) then writeln('inter-configuration formers:');
for i:=1 to _form_intr do
    imtag_produce_code(data,form_intr[i,1],form_intr[i,2]);

writeln;
writeln('[imtag]: done');
writeln;

//writeln('ALLFLOW: ');
//writeln(wrap(imtag_make_flow_calc(flow_intr_calc,'A($)'),8,8));

//writeln('DIRS: ',flow_intr_dirs);

//writeln('confdirs: ',wrap(imtag_make_flow_dirs(flow_conf_dirs,' # = $'),8,8));
//writeln('confdirs 2: ',wrap(imtag_make_flow_dirs(flow_conf_dirs,' #'),8,8));

end.
