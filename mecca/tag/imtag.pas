// en FPC >= 2.2.0

// = imdouble.pas ====================================================
// isotopic mechanism tagging tool
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

{$DEFINE xCRT_INTEQN}  // defines whether to create an additional
                      // equation file with isotopologues
                      // interchange reactions

{$DEFINE MAKE_DOTS}   // defines whether to create .dot files for
                      // graphvis showing species interrelation

{$DEFINE OPT_FINAL}   // final optimizations introduced

// -----------------------------------------------------------------

//  linking common utils/types include file for im-double/tag
{$I imcom.inc}

// -----------------------------------------------------------------

// - TAGGING PART --------------------------------------------------

// non-intermediate species
var  nis : array[1..max_tsl] of string;
    _nis : word;

// make a list of non-intermediate species
procedure make_nis;
var s, i, j : word;
    f : boolean;
begin
writeln('non-intermediate species:');
_nis:=0;
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
       inc(_nis);
       nis[_nis]:=tsl[s].spec;
       end;
    end;

writeln('#',_nis,' species: ');
for i:=1 to _nis do
    write(nis[i],' ');
writeln;
writebreak;
end;

// returns true if a species is not an intermediate
function in_nis(spec : string) : boolean;
var i : word;
begin
in_nis:=false;
for i:=1 to _nis do
    if (spec=nis[i]) then
       begin in_nis:=true; break; end;
end;

// -----------------------------------------------------------------

const max_reac = 300;  // maximum amount of reactions containing
                       // one selected species (increase if necessary)

// tagging record for a set of species
type ttagset = array[1..max_tsl] of record
     spec : nstr;                             // species name
    _prod : integer;                          // & # of reactions
     prod : array[1..max_reac] of integer;    // reaction #
     prec : array[1..max_reac] of string;     // who produced
     stoi : array[1..max_reac] of real;       // & corr. stoichiometric coeff

    _sink : integer;
     sink : array[1..max_reac] of integer;
     is2n : array[1..max_reac] of boolean;    // sinks to nothing?
     isd2 : array[1..max_reac] of boolean;    // sinks doubly?

    _akie : integer;                          // no of KIEs associated
     akie : array[1..max_reac] of integer;    // +/- of abs kie r-n index in imcom
     skie : array[1..max_reac] of integer;    // no of the isotopologue

     ifix : boolean;                          // is fixed?
     end;

// - tagit ---------------------------------------------------------------------
// investigate the tagging information of the
// current mechanism and allowed tagged species
procedure tagit(var d : ttagset);

var i, j : integer;
    e, r, s, p  : integer;
    qae : array[1..2] of integer;
    af : real;

begin

// make non-intermediate species list
make_nis;

writeln('taginfo:');

// initializing
fillchar(d,sizeof(d),0);

for s:=1 to _utsl do         // cycle through tagged species found in mech.
    with d[s] do

    begin

    // initialize record for each
    spec:=tsl[utsl[s]].spec;
   _prod:=0;
    fillchar(prod,sizeof(prod),0);
    fillchar(prec,sizeof(prec),0);

   _sink:=0;
    fillchar(sink,sizeof(sink),0);

   _akie:=0;
    fillchar(akie,sizeof(akie),0);
    fillchar(skie,sizeof(skie),0);

    ifix:=tsl[s].ifix;

//    if (ifix) then continue;             // skipping the fixed species?

    for r:=1 to _eqs do // cycle through reactions list
        if (eqs[r].itag) then          // checking "dedicated for tagging" flag (additional)
           begin

           // so far skipping exchange reactions
           if (eqs[r].iiex) then continue;
   
           // inspecting sink
           if ((eqs[r].educ[1]=spec) or (eqs[r].educ[2]=spec)) then
              begin
              inc(_sink);
              sink[_sink]:=r;

              // checking if sinks doubly
              isd2[_sink]:=(eqs[r].educ[1]=eqs[r].educ[2]);

              // checking if it sinks to nothing
              is2n[_sink]:=true;
              for i:=1 to eqs[r]._prod do
                  if (in_tsl(eqs[r].prod[i])) then
                     begin
                     is2n[_sink]:=false;
                     break;
                     end;
              end;

           // inspecting production
           for p:=1 to eqs[r]._prod do        
               if (eqs[r].prod[p]=spec) then  // if a species is found to be produced in this r-n
                  if (eqs[r].isrc) then       // source-specified reaction, treating separately
                     for e:=1 to src[eqs[r].nsrc]._trans do     // searching through all specified transfer records
                         for j:=1 to src[eqs[r].nsrc].trans[e]._dst do     // searching in current transfer destinations list
                             if (src[eqs[r].nsrc].trans[e].dst[j].spec=spec) then
                                begin
                                inc(_prod);
                                prod[_prod]:=r;
                                stoi[_prod]:=src[eqs[r].nsrc].trans[e].dst[j].stoi*   // branching coeff. from src
                                             eqs[r].stoi[p];                          // product's self stoi
                                prec[_prod]:=src[eqs[r].nsrc].trans[e].src;
                                end
                             else 
                  else  {if isrc}             // regular r-n, mass/composition weighted transfer probability
                      for e:=1 to 2 do // cycle through educts to determine whether one is tagged (or both!)                  
                          if (in_tsl(eqs[r].educ[e])) then
                             begin

                             // q-ty of atoms in educts
                             qae[e]:=tsl[no_tsl(eqs[r].educ[e])].qatm;
                             if (in_tsl(eqs[r].educ[3-e])) then 
                                qae[3-e]:=tsl[no_tsl(eqs[r].educ[3-e])].qatm
                             else
                                 qae[3-e]:=0;

                             // accounting mass/comp. transfer balance as mass-weighted share of the products
                             af:=qae[e]/(qae[1]+qae[2]);

                             // if compound reacts with itself,
                             if (eqs[r].educ[1]=eqs[r].educ[2]) then
                                if (e=2) then  // avoiding double accounting processing 2nd educt
                                   continue
                                else           // transfer should be straight since the same educt
                                    af:=1.0;

                             // adding new product record
                             inc(_prod);
                             prod[_prod]:=r;
                             stoi[_prod]:=af*eqs[r].stoi[p];
                             prec[_prod]:=eqs[r].educ[e];
                             end;
   
           end;
   
    writeln('-',spec,'-------------------------------------------------------------- ');

    write('S (',_sink,'): ');
    for i:=1 to _sink do
        begin
        for j:=1 to _kie do
            if (kie[j].imec) then
               if (eqs[sink[i]].abbr=kie[j].abbr) then
                  for p:=1 to _isos do
                      if (kie[j].isot=tsl[no_tsl(spec)].isos[p]) then
                         begin
                         inc(_akie);
                         akie[_akie]:=-j;
                         skie[_akie]:=p;
                         write('KIE[',kie[j].isot,']~');
                         end;
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
                  for p:=1 to _isos do
                      if (kie[j].isot=tsl[no_tsl(prec[i])].isos[p]) then
                         begin
                         inc(_akie);
                         akie[_akie]:=j;
                         skie[_akie]:=p;
                         write('KIE[',kie[j].isot,']~');
                         end;

        if not(stoi[i]=1.0) then
           write(floattostr(stoi[i]),'*');

        write(eqs[prod[i]].abbr);

        write('[',prec[i],']  ');
        end;
    
    writeln;

    end;

writeln;
writeln('tagit(',paramstr(1),',',paramstr(2),'): done');

end;


// - CODE-CREATING PART --------------------------------------------------------

var flow_conf_calc : ansistring; // flow calculation code as template
    flow_conf_dirs : ansistring; // number of direction blocks to calculate (i.e. counting CH4_sink, CH4_CH3O2, etc.)
    flow_conf_ecnt : integer;    // directions list
    flow_conf_ckie : ansistring; // kie-correction code for each configuration
    flow_conf_ciex : ansistring; // iex-correction code for each configuration

var flow_intr_calc : ansistring;  // same as above but for all configurations
    flow_intr_dirs : ansistring;
    flow_intr_ecnt : integer;

var jac_conf_calc : ansistring;   // jacobian computation code for configuration
    jac_conf_ecnt : integer;      // no. of nonzero elements

type // spec. jacobian matrix for simplified linear computation -- using heap
    tjacelm = array[1..max_tsl,1..max_tsl] of string[200];
var jacelm : ^tjacelm;


// adds new direction to the current and inter-configuration directions list
procedure flow_add_safe(_from, _to, _code : string);
var c : string;
begin

// direction mark
c:='/'+_from+'>'+_to+':';

// to current
inc(flow_conf_ecnt);
flow_conf_dirs+=c;
flow_conf_calc+=_code+_LF;

// to inter-configuration flow code
if (pos(c,flow_intr_dirs)=0) then
   begin
   inc(flow_intr_ecnt);
   flow_intr_dirs+=c;
   flow_intr_calc+=_code+_LF;
   end;

end;


// prepare_flow_calc -----------------------------------------------------------
// code for species2species molecules "flow" and
// related elements (jacobian, coeff. matrix) calculation
procedure prepare_flow_jac_calc(d : ttagset);
//procedure write_isot_flow;
var s, r, j, i : word;
    a, b, c, done, p : ansistring;
begin


getmem(jacelm,sizeof(tjacelm));
fillchar(jacelm^,sizeof(tjacelm),0);

// calculation code with templates
flow_conf_calc:='  ! ----- '+tagname+': flow(FROM_TO[sink]) = SUM( PTs(FROM->TO[sink]) ) -----'+_LF;
flow_conf_ecnt:=0;    // number of direction blocks to calculate (i.e. counting CH4_sink, CH4_CH3O2, etc.)
flow_conf_dirs:='';   // directions list                                                                  
flow_conf_ckie:='';   // kie-corr code
flow_conf_ciex:='';   // kie-corr code

jac_conf_calc:='';
jac_conf_ecnt:=0;

for s:=1 to _utsl do
    with d[s] do
         begin

         p:=_LF;
         p+='  ! _____ '+cfgname+': '+spec+' _____';
         if (ifix) then
            p+=' WARN: indicated as fixed species';
         p+=_LF;

         // inspecting sink
         if (_sink>0) then

            begin
            a:=p+'    flow('+spec+'_sink) = -( ';                                       // flow_sink: "-" used to be "+" here
            b:='';
            c:='';
            for r:=1 to _sink do
                begin
                if (is2n[r]) then 
                   begin
                   if (c<>'') then c+=' + ';
                   c+='{'+eqs[sink[r]].abbr+'}';  // sink to nothing
                   if (isd2[r]) then c:='2.0_dp * '+c;                   // double-sink to nothing
                   end;
                if (isd2[r]) and not(is2n[r]) then                      // if double-sinks and not to nothing
                   begin
                   // enumerating double-sink reactions
                   // in case compound reacts with itself, 2 molecules are consumed
                   b+='{'+eqs[sink[r]].abbr+'}';
                   b+=' + ';                     // for a next one
                   end
                else
                    begin
                    // storing reaction name (cannot get an internal number 'cause it can change with next conf.)
                    a+='{'+eqs[sink[r]].abbr+'}';
                    a+=' + ';                    // for a next sink
                    end;
                end;

            setlength(a,length(a)-2);       // cut last plus "+"

            if (b<>'') then
               begin
               setlength(b,length(b)-2);       // cut last plus "+"
               
               // adding doubled sink to all
               a+='+ 2.0_dp * ( '+b+' )';
               end;
               
            a+=')';

{$xIFNDEF OPT_FINAL}
            // add a sink flow entry to current- and inter-configuration flow code
            flow_add_safe(spec,'sink',a);
{$xENDIF}

{$IFDEF CRT_INTEQN}
            if (b<>'') then
               begin
               // adding new flow entry for the double sink
               b:='    flow('+spec+'_sink2) = ( '+b+' )';
               flow_add_safe(spec,'sink2',b);
               end;
            if (c<>'') then
               begin
               // adding new flow entry for the sink to nothing
               c:='    flow('+spec+'_sinkN) = ( '+c+' )';
               flow_add_safe(spec,'sinkN',c);
               end;
{$ENDIF}

            // jacobian diagonal element - sink
            jacelm^[no_tsl(spec),no_tsl(spec)]:=jacelm^[no_tsl(spec),no_tsl(spec)]+
                                               'flow('+spec+'_sink)';                   // flow_sink: "-flow" used to be

            end;

         // inspecting production
         if (_prod>0) then
            begin

            done:='';
            b:='';

            // collecting reactions for the same species
            for r:=1 to _prod do
                if (not(pos('>'+prec[r]+'<',done)>0)) then
                   begin

                   a:=''; // regular components part
                   c:=''; // temp
                   for j:=1 to _prod do
                       if (prec[j]=prec[r]) then
                          begin

                          c:='';
                          if (stoi[j]<>1.0) then
                             c:=formatfloat('##0.0###############',stoi[j]) + ' * ';     {floattostr}

                          // storing reaction name and internal number
                          c+='{'+eqs[prod[j]].abbr+'}';
                          a:=a+c+' + ';
                          end;

                   setlength(a,length(a)-2);       // remove last plus "+"
//                 a:='( '+a+')';                  // into the braces

                   a:='    flow('+prec[r]+'_'+spec+') = '+a;
                   if (_sink=0) then a:=p+a;

{$IFDEF CRT_INTEQN}
                   // adding new production flow entry for the total production
                   if (b<>'') then b+=' + ';
                   b+='flow('+prec[r]+'_'+spec+')';
{$ENDIF}
                   done+='>'+prec[r]+'<';

                   // jacobian
                   jacelm^[no_tsl(spec),no_tsl(prec[r])]:=jacelm^[no_tsl(spec),no_tsl(prec[r])]+
                                                          'flow('+prec[r]+'_'+spec+')'; // flow_sink: "+flow" used to be

                   // add flow entry for production
                   flow_add_safe(prec[r],spec,a);

                   end;

{$IFNDEF OPT_FINAL}
{$IFDEF CRT_INTEQN}
            if (b<>'') then
               begin
               b:='    flow('+spec+'_prod) = '+b;
               flow_add_safe(spec,'prod',b);
               end;
{$ENDIF}
{$ENDIF}
            end;

         end;

// making KIE corrections calculation former
flow_conf_ckie:='  ! ----- '+tagname+': flow(KIE_REAC_ISOT) corrections calculation -----'+_LF+_LF;
for j:=1 to _kie do
    if (kie[j].imec) and not(eqs[no_eqs(kie[j].abbr)].iiex) then
       begin
           
       c:=kie[j].abbr+'_'+kie[j].isot;
       flow_conf_ckie+='    flow(KIE_'+c+') = '+
                       '{'+kie[j].abbr+'} * (1.0_dp - 1.0_dp'+kie[j].expr+')'+_LF;
                  
       flow_conf_dirs+='/'+'KIE'+'>'+c+':';            // KIE corrections are included
       flow_intr_dirs+='/'+'KIE'+'>'+c+':';            // to the directions list
                  
       inc(flow_conf_ecnt);
       inc(flow_intr_ecnt);
                  
       end;

// making IEX corrections calculation former
flow_conf_ciex:='  ! ----- '+tagname+': flow(IEX_REAC_ISOT) corrections calculation -----'+_LF+_LF;
for s:=1 to _iex do
    if (iex[s].imec) then
       for r:=1 to 2 do            // cycling through exchanging species   
           for i:=2 to _isos do    // cycling through minor isotopologues
               begin
           
               c:=iex[s].abbr+'_'+tsl[iex[s].exspec[r]].isos[i];
               
               flow_conf_ciex+='    flow(IEX_'+c+') = '+
                               '('+inttostr(tsl[iex[s].exspec[3-r]].qatm)+'.0_dp/'+                      // transfer probability
                                   inttostr( tsl[iex[s].exspec[r]].qatm+tsl[iex[s].exspec[3-r]].qatm )+'.0_dp)*'+
                               '{'+iex[s].abbr+'}';

               for j:=1 to _kie do
                   if (kie[j].imec) and (kie[j].abbr=iex[s].abbr) then
                      if (kie[j].isot=tsl[iex[s].exspec[3-r]].isos[i]) then
                         flow_conf_ciex+=kie[j].expr;
                         
               flow_conf_ciex+=_LF;

               flow_conf_dirs+='/'+'IEX'+'>'+c+':';            // KIE corrections are included
               flow_intr_dirs+='/'+'IEX'+'>'+c+':';            // to the directions list

               inc(flow_conf_ecnt);
               inc(flow_intr_ecnt);
               end;

// forming items of the jac matrix
jac_conf_ecnt:=0;
for s:=1 to _utsl do        // source
    for r:=1 to _utsl do    // receiver
        if (length(jacelm^[utsl[r],utsl[s]])>0) then
           begin
           // syntax:  "/sink>recv=flow-expr:"
           jac_conf_calc+='/'+tsl[utsl[s]].spec+'>'+tsl[utsl[r]].spec+'='+
                          jacelm^[utsl[r],utsl[s]]+':';   
           inc(jac_conf_ecnt);
           end;

freemem(jacelm,sizeof(tjacelm));

writeln('prepare_flow: done');
writeln('  flow_calc: ',flow_conf_ecnt,' entries (all flow: ',flow_intr_ecnt,')');
writeln('  jacobian: ',jac_conf_ecnt,' entries');

end;

// makes the code for flow calculation according to the sample
function imtag_make_flow_calc(flowstr, ptvalstr : string) : ansistring;
var smp, out : ansistring;
    i, j : integer;
begin

out:=flowstr;
if (ptvalstr='') then ptvalstr:='C(ind_'+ptsyntax+')';

for i:=1 to _eqs do
    begin
    smp:=substr(ptvalstr,'@',eqs[i].abbr);
    smp:=substr(smp,'$',inttostr(eqs[i].ntag));
    out:=ansireplacetext(out,'{'+eqs[i].abbr+'}',smp);
    end;
    
imtag_make_flow_calc:=wrap(out,6,6);

end;

// makes a list of the flow directions according to the sample
function imtag_make_flow_dirs(dirstr, sample : ansistring) : ansistring;
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

      smp:=substr(sample,'@',sink+'_'+recv);
      smp:=substr(smp,'$',inttostr(dirno));
      
      out+=smp+',';
      inc(dirno);
      end;

setlength(out,length(out)-1);  // cut last comma
out:=trim(out);                // cut leading spaces

imtag_make_flow_dirs:=out;

end;

// makes a list of the flow sources according to the sample
function imtag_make_flow_srcs(dirstr, sample : ansistring) : ansistring;
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

      smp:=substr(sample,'@',substr(sisyntax,'@',sink));
      smp:=substr(smp,'$',inttostr(dirno));
      
      out+=smp+',';
      inc(dirno);
      end;

setlength(out,length(out)-1);  // cut last comma
out:=trim(out);                // cut leading spaces

out:='(/ '+out+' /)';          // making in F array syntax

imtag_make_flow_srcs:=out;

end;



// d : tagging setup; tname, fname : former (template) and output filenames
procedure imtag_parse_code(d : ttagset; tname, fname : string);

var f : text;  // output file handle (all procs write to f)

// write_isot_derivs -----------------------------------------------------------
// code for selected isotopologue derivatives calculation
procedure make_isot_derivs(derisot, _sdest, _ssink, _sflow : string; abuinrare : boolean);

var s, r, j, k, i : word;
    a, b, z, p, done : ansistring;
    det_isot : boolean;

begin

det_isot:=false;
for i:=1 to _isos do
    if ((clsname[i]+isoatom)=derisot) then
       begin
       det_isot:=true;
       break;
       end;
if not(det_isot) then
   begin
   writeln(' make_isot_derivs: could not detect isotopologue (',derisot,'), check the former');
   exit;
   end;

writeln(f,'    ! ===== ',tagname,': ',_sdest,' <- ',_ssink,_sflow,' + ... ) =====');

for s:=1 to _utsl do
    with d[s] do
         begin

         writeln(f);

         write(f,'      ! _____ ',spec,' _____');
         if (ifix) then
            write(f,'  WARN: indicated as fixed species');
         writeln(f);

         a:='';

         // inspecting sink
         if (_sink>0) then
            begin
            a:=' + '+{format('%18s',[}substr(_ssink,'$',spec){])};              // flow_sink: "+" used to be "-" here
            b:='flow('+spec+'_sink)';

            // checking if there is KIE in sink
            for r:=1 to _sink do
                for k:=1 to _kie do
                    if (kie[k].imec) and (eqs[sink[r]].abbr=kie[k].abbr) then
                       if (tsl[utsl[s]].isos[i]=kie[k].isot) then
                          begin
                          p:='flow(KIE_'+kie[k].abbr+'_'+kie[k].isot+')';

                          // in case compound reacts with itself, 2 molecules are consumed
                          if (eqs[sink[r]].educ[1]=eqs[sink[r]].educ[2]) then
                             p:='2.0_dp * '+p;

                          // adding KIE-correction for sink
                          p:=' + '+p;                                           // flow_sink: "+" used to be "-" here
                          b:=b+p;
                          end;
            
            a+=substr(_sflow,'~','( '+b+' )');

            end;

         z:='';

         // inspecting production
         if (_prod>0) then
            begin


            done:='';

            for r:=1 to _prod do
                if (not(pos('>'+prec[r]+'<',done)>0)) then
                   begin
                   done+='>'+prec[r]+'<';
                   
                   // skipping entries for molecules of 1 selected isotope atom
                   // when transferring abundant in rare
                   if (abuinrare) and (tsl[no_tsl(prec[r])].qatm=1) then 
                      begin
                      z+=prec[r]+'('+inttostr(tsl[no_tsl(prec[r])].qatm)+') ';
                      continue;
                      end;
                   
                   if (a<>'') then a+='&'; // LF for each new string

                   a+=' + '+{format('%18s',[}substr(_ssink,'$',prec[r]){])};
                   b:='flow('+prec[r]+'_'+spec+')';

                   // checking whether the reaction has kie
                   for j:=1 to _prod do
                       if (prec[j]=prec[r]) then
                          for k:=1 to _kie do
                              if (kie[k].imec) and (eqs[prod[j]].abbr=kie[k].abbr) then
                                 if (tsl[no_tsl(prec[r])].isos[i]=kie[k].isot) then
                                    begin
                                    p:='flow(KIE_'+kie[k].abbr+'_'+kie[k].isot+')';

                                    // in case the stoichiometric coefficient is != 1
                                    // additional braces for numerical reasons
                                    if (stoi[j]<>1.0) then
                                       p:=formatfloat('##0.0###############',stoi[j])+' * '+p;  {floattostr}

                                    // subtracting KIE-correction for production
                                    p:=' - '+p;
                                    b:=b+p;
                                    end;
                   a+=substr(_sflow,'~','( '+b+' )');

                   end;

            end;

         if (a<>'') then
            begin
            // making destination
            a:=substr(_sdest,'@',spec)+' = &'+a;
            // and output
            writeln(f,wrap(a,33,8));
            end
         else
             writeln(f,'      ! no active sink/production');

         if (z<>'') then 
            writeln(f,'      ! skipped [abu in min]: ',z);

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
             if ((kie[k].imec) and not(eqs[no_eqs(kie[k].abbr)].iiex)) then
                if (pos(kieisot,kie[k].isot)>0) then  // if an isoclass sign. is found in isot.spec. of KIE
                   with eqs[kie[k].eqno] do
                        if (educ[1]=sink) or (educ[2]=sink) then    // if sink is among the educts of the eqs
                           if (recv=sink) then                      // if jac entry accounts sink (i.e. diagonal)
                              begin
                              a:='flow(KIE_'+kie[k].abbr+'_'+kie[k].isot+')';
                              if (educ[1]=educ[2]) then             // in case if compound reacts with itself
                                 a:='2.0_dp*'+a;  
                              a:='+'+a;                             // in case of sink we SUM the correction!
                              end
                           else                                     // non-diagonal element
                               for i:=1 to _prod do
                                   if (prod[i]=recv) then
                                      begin
                                      a:='flow(KIE_'+kie[k].abbr+'_'+kie[k].isot+')';
                                      if (stoi[i]<>1.0) then        // in case if compound has a stoichiom. coeff.
                                         a:=floattostr(stoi[i])+'_dp*'+a;
                                      a:='-'+a;                     // in case of production we SUBTRACT the correction!
                                      end
         end;
        
      if ((kieisot<>'') and (a='')) then continue
                                    else expr+=a;
                  
      smp:=substr(sample,'~',expr);
      smp:=substr(smp,'@',sink);
      smp:=substr(smp,'$',recv);

      out+=smp+_LF;
      end;

imtag_make_jac_calc:=out;

end;

//
//
function imtag_make_iex_proc(pair, expr, dest, iexisot : string) : ansistring;
var tmp, out, a : ansistring;
    r, rs, i, j, k : integer;
    det_isot : boolean;
begin

//      NIEX(%,k)
  //    ~*FR({%TAG}_#,k)*FA({%TAG}_$)
    //  DIEX(#,k)

det_isot:=false;
for k:=2 to _isos do
    if ((clsname[k]+isoatom)=iexisot) then
       begin
       det_isot:=true;
       break;
       end;
if not(det_isot) then
   begin
   writeln(' make_iex_calc: could not detect isotopologue (',iexisot,'), check the former');
   exit;
   end;

// assessing net exchange rates betw. species
rs:=0;                  // straight r-n no
out:='';
for r:=1 to _iex do
    if (iex[r].imec) then
       begin
       inc(rs);
       
       tmp:=substr(expr,'@',tsl[iex[r].exspec[1]].spec);
       tmp:=substr(tmp,'$',tsl[iex[r].exspec[2]].spec);
       tmp:=substr(tmp,'~','+flow(IEX_'+iex[r].abbr+'_'+tsl[iex[r].exspec[2]].isos[k]+')');
       a:=tmp;

       tmp:=substr(expr,'@',tsl[iex[r].exspec[2]].spec);
       tmp:=substr(tmp,'$',tsl[iex[r].exspec[1]].spec);
       tmp:=substr(tmp,'~','-flow(IEX_'+iex[r].abbr+'_'+tsl[iex[r].exspec[1]].isos[k]+')');
       
       tmp:=substr(pair,'&',inttostr(rs))+' = '+a+tmp+_LF;

       out+=wrap(tmp,length(pair)+3,4);
       end;

out+=_LF;

// forming echange calculation code

for j:=1 to _iesl do
    begin
    
    tmp:=substr(dest,'@',tsl[iesl[j]].spec);
    tmp+=' = ';
    
    for i:=1 to _iex do
        if (iex[i].imec) then
           if (iex[i].exspec[1]=iesl[j]) then    
              tmp+='-'+trim(substr(pair,'&',inttostr(i)))
           else
               if (iex[i].exspec[2]=iesl[j]) then
                  tmp+='+'+trim(substr(pair,'&',inttostr(i)));
                  
    out+=wrap(tmp,length(pair)+3,4)+_LF;
    end;        
               
imtag_make_iex_proc:=out;

end;


// imtag_fudgecode ---------------------------------------------------
// write "fudging" code to tune to the doubled mechanism
procedure imtag_write_fudgecode(sample : ansistring);
var i, j : word;
    tmp : ansistring;
begin

for i:=1 to _utsl do
    begin
    tmp:=substr(sample,'@',d[i].spec);
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

write('imtag_parse_code(',fname,'): ');

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
             writeln(f,imcom_make_tracdecl(imcom_ext4marks(a,'[%','%]')))
          else
          if pos('{$TAG_SPECS}',a)>0 then
             writeln(f,imcom_make_tagspecs_list(imcom_ext4marks(a,'[%','%]')))
          else
          if pos('{$SPECS_LIST}',a)>0 then  // doubles previous function avoiding formatting like ' ',' ',' '
             writeln(f,imcom_make_specslist(imcom_ext4marks(a,'[%','%]')))
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
                 if (pos(clsname[i]+isoatom,a)>0) then
                      make_isot_derivs(clsname[i]+isoatom,
                                       imcom_ext4marks(a,'[%','%]'),
                                       imcom_ext4marks(a,'(%','%)'),
                                       imcom_ext4marks(a,'<%','%>'),
                                       (pos('^',a)>0));
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
          if pos('{$FLOW_DIRS}',a)>0 then        // flow directions
             if pos('INT',uppercase(a))>0 then   // inter-configuration
                writeln(f,wrap('    '+imtag_make_flow_dirs(flow_intr_dirs,
                                                           imcom_ext4marks(a,'[%','%]')),4,4))
             else
                 writeln(f,wrap('    '+imtag_make_flow_dirs(flow_conf_dirs,
                                                            imcom_ext4marks(a,'[%','%]')),4,4))
          else
          if pos('{$FLOW_SRCS}',a)>0 then        // flow sources
             if pos('INT',uppercase(a))>0 then   // inter-configuration
                writeln(f,wrap('    '+imtag_make_flow_srcs(flow_intr_dirs,
                                                           imcom_ext4marks(a,'[%','%]')),7,4))
             else
                 writeln(f,wrap('    '+imtag_make_flow_srcs(flow_conf_dirs,
                                                            imcom_ext4marks(a,'[%','%]')),7,4))
          else      
          if pos('{$JAC_CALC}',a)>0 then
             writeln(f,imtag_make_jac_calc(imcom_ext4marks(a,'[%','%]'),''))
          else
          if pos('{$JAC_KIE',a)>0 then
             for i:=1 to _isos do
                 if (pos(clsname[i]+isoatom,a)>0) then
                    writeln(f,imtag_make_jac_calc(imcom_ext4marks(a,'[%','%]'),
                                                  clsname[i]+isoatom))
                 else
          else
          if (pos('{$KIE_CALC}',a)>0) then
             writeln(f,imtag_make_flow_calc(flow_conf_ckie,
                                            imcom_ext4marks(a,'[%','%]')))
          else
          if (pos('{$IEX_CALC}',a)>0) then
             writeln(f,imtag_make_flow_calc(flow_conf_ciex,
                                            imcom_ext4marks(a,'[%','%]')))
          else
          if (pos('{$KIE_PROC}',a)>0) then
             write(f,imcom_parse_proc(kieproc))
          else
          if (pos('{$IEX_PROC',a)>0) then
             for i:=2 to _isos do
                 if (pos(clsname[i]+isoatom,a)>0) then
                    writeln(f,imtag_make_iex_proc(imcom_ext4marks(a,'[%','%]'),
                                                  imcom_ext4marks(a,'(%','%)'),
                                                  imcom_ext4marks(a,'<%','%>'),
                                                  clsname[i]+isoatom))
                 else
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

end;   { imtag_parse_code }

// -----------------------------------------------------------------

// original equation file with added passive tracers
procedure imtag_produce_eqnPTs_file(fname : string);

var f : textfile;
    i, j : word;
    a, p, tmp : ansistring;

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
            if (itag) then
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
               if (pos('>'+substr(ptsyntax,'@',abbr)+'<',p)=0) then
                  // adding production tracer
                  a:=a+substr(ptsyntax,'@',abbr)+' '
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
writeln(f,'{ configuration: ',cfgname,' }');
{$IFDEF USE_PT}
writeln(f,'{ # of added passive tracers: ',_ptracs_intr,' }');
{$ELSE}
writeln(f,'{ running imtag version was compiled with USE_PT switched off }');
writeln(f,'{ passive tracers were NOT created                            }');
{$ENDIF}
writeln(f);

{$IFDEF USE_PT}
writeln(f,'{- passive production tracers ------------------------------------------------}');
writeln(f);

for i:=1 to _eqs do
    with eqs[i] do
         if (itag) then
            safeaddspec(substr(ptsyntax,'@',abbr),'IGNORE; { '+abbr+' reaction passive production tracer }');

writeln(f);
{$ENDIF}

writeln(f,'{-------------- [imtag] - end ------------------------------------------------}');
writeln(f);

close(f);

if (declspecs<>'') then write('(warning, declined as already existing species: ',declspecs,') ');

writeln('done');

end;

// -----------------------------------------------------------------

// TAG@MECCA equation file
procedure imtag_produce_eqnINT_file(d : ttagset; fname : string);

var e : textfile;
    s, r, j, k, i : integer;
    a, b, c, p, v, done : ansistring;
    qap, qae, pf : real;
    sink2, sinkN : boolean;

begin

write('produce_eqnINT_file(',fname,'): ');

assign(e,fname);
rewrite(e);

writeln(e,imcom_parse_proc(eqnproc));

writeln(e);

writeln(e,'#EQUATIONS');
writeln(e);

{$DEFINE xSPUNI}

for s:=1 to _utsl do
with d[s] do
     begin
     write(e,'{ _____ ',spec,' _____ ');
     if (ifix) then
        write(e,' WARN: indicated as fixed species ');
     writeln(e,'}');
     
         begin

         a:='';

{$IFNDEF OPT_FINAL}
         // concentration r-n
         if (_sink>0) or (_prod>0) then
            begin
            a:=' <'+cfgname+'_'+spec+'> ';
            a+='UNITY = A'+spec+' : {%'+cfgname+'} ';
            if (_sink>0) then
               a+='flow('+spec+'_sink)';
            if (_prod>0) then
               begin
               if (_sink>0) then a+=' + ';
               a+='flow('+spec+'_prod)';
               end;
            a+=';';
            writeln(e,a);
            end;
{$ENDIF}    
         
         a:='';

         // inspecting sink
         // writing down double sink r-ns
         if (_sink>0) then
          for i:=1 to _isos do
            begin

     // spec sum
     v:='';
     for j:=1 to _isos do
         v+='C('+substr(sisyntax,'@',tsl[utsl[s]].isos[j])+')+';
     setlength(v,length(v)-1);

            sink2:=false;
            sinkN:=false;
            
{$IFDEF SPUNI}
            b:='flow('+spec+'_sink)';
{$ELSE}
            b:='flow('+spec+'_sink2)';
            c:='flow('+spec+'_sinkN)';
{$ENDIF}

            // checking sink2 and if there is KIE in it
            for r:=1 to _sink do
                begin

                // sinking to nothing species
                if (is2n[r]) then sinkN:=true
                             else // if sinks doubly
                                  if (isd2[r]) then sink2:=true;
                // checking KIE
                for k:=1 to _kie do
                    if (kie[k].imec) and (eqs[sink[r]].abbr=kie[k].abbr) then
                       if (tsl[utsl[s]].isos[i]=kie[k].isot) then
                          begin
                          p:='flow(KIE_'+kie[k].abbr+'_'+kie[k].isot+')';
                          if (isd2[r]) and (is2n[r]) then p:='2.0_dp*'+p;

                          // summing for regular sink (since it is negative) ->   p:=' + '+p;
                          p:=' - '+p;
                          if (is2n[r]) then 
                             c:=c+p
                          else
                              b:=b+p;
                          
                          end;
                end;
{$IFNDEF SPUNI}
 
            if (sink2) then
               begin
               a:=' <'+cfgname+'_'+tsl[utsl[s]].isos[i]+'_sink2> ';
               a+=tsl[utsl[s]].isos[i]+' = SINK : {%'+cfgname+'} ';
//             a+=tsl[utsl[s]].isos[i]+' + INV_'+spec+' = SINK : {%'+cfgname+'} ';
               a+='safediv(( '+b+' ), '+v+' );';
//             a+='( '+b+' );';
               writeln(e,a);
               end;

            if (sinkN) then
               begin
               a:=' <'+cfgname+'_'+tsl[utsl[s]].isos[i]+'_sinkN> ';
               a+=tsl[utsl[s]].isos[i]+' = SINK : {%'+cfgname+'} ';
//             a+=tsl[utsl[s]].isos[i]+' + INV_'+spec+' = SINK : {%'+cfgname+'} ';
               a+='safediv(( '+c+' ), '+v+' );';
//             a+='( '+c+' );';
               writeln(e,a);
               end;

{$ELSE} // SPUNI
            if (i>1) then
               begin
               a:=' <'+cfgname+'_'+tsl[utsl[s]].isos[i]+'_sink> ';
               a+=tsl[utsl[s]].isos[i];
               a+=' = SINK : {%'+cfgname+'} ';            
               a+='safediv('+b+','+v+');';
               writeln(e,a);
               end;
{$ENDIF} // SPUNI

            end
         else
             if (i=1) then writeln(e,'{ '+spec+' does not sink this mech }');

         // inspecting production
         if (_prod>0) then
          for i:=1 to _isos do
            begin

            done:='';
            a:='';

            for r:=1 to _prod do
                if (not(pos('>'+prec[r]+'<',done)>0)) then
                   begin

     // spec sum
     v:='';
     for j:=1 to _isos do
         v+='C('+substr(sisyntax,'@',tsl[no_tsl(prec[r])].isos[j])+')+';
     setlength(v,length(v)-1);


                   done+='>'+prec[r]+'<';
                   
                   b:='flow('+prec[r]+'_'+spec+')';

                   // checking whether the reaction has kie
                   for j:=1 to _prod do
                       if (prec[j]=prec[r]) then
                          for k:=1 to _kie do
                              if (kie[k].imec) and (eqs[prod[j]].abbr=kie[k].abbr) then
                                 if (tsl[no_tsl(prec[r])].isos[i]=kie[k].isot) then
                                    begin
                                    p:='flow(KIE_'+kie[k].abbr+'_'+kie[k].isot+')';

                                    // in case the stoichiometric coefficient is != 1
                                    // additional braces for numerical reasons
                                    if (stoi[j]<>1.0) then
                                       p:=formatfloat('##0.0###############',stoi[j])+' * '+p;  {floattostr}

                                    // subtracting for production
                                    p:=' - '+p;
                                    b:=b+p;
                                    end;

{$IFNDEF SPUNI}
                   a:=' <'+cfgname+'_'+tsl[utsl[s]].isos[i]+'_'+prec[r]+'> ';
                   a+=tsl[no_tsl(prec[r])].isos[i];
//                 a+=tsl[no_tsl(prec[r])].isos[i]+' + INV_'+prec[r];
                   a+=' = ';
                   
                   qap:=tsl[utsl[s]].qatm;          // quantity of the atoms in product
                   qae:=tsl[no_tsl(prec[r])].qatm;  //  --"--                in educt
                   pf:=qap/qae;                     // prob = A(prod)/A(educ), i.e. 1/3 for C3H6 -> CH3O2

                   if (spec = prec[r]) then
                      a+='2 ';                      // adding 2 to the specs producing itself (kpp issue)
                   
                   // if # of atoms in product = # atoms in educt: straightforwardly creating minor isotopologue
                   // if # of atoms in product > # atoms in educt: adding atoms from major pool
                   // if # of atoms in product < # atoms in educt: freeing abundant excess to the major pool
                   if not(pf=1.0) and (i>1) then    // considering minor isotpologues
                      begin
                      a+=formatfloat('##0.0###############',pf)+' '+tsl[utsl[s]].isos[i];
                      if ((1.0-pf)<0) then a+=' - ' else a+=' + ';
                      a+=formatfloat('##0.0###############',abs(1.0-pf))+' '+tsl[utsl[s]].isos[1];
                      end
                   else
                       a+=tsl[utsl[s]].isos[i];
                   a+=' : {%'+cfgname+'} ';
//                 a+='( '+b+' );';
                   a+='safediv(( '+b+' ), '+v+' );';
                   writeln(e,a);
{$ELSE} // SPUNI
                   if (i>1) then
                      begin
                      a:=' <'+cfgname+'_'+tsl[utsl[s]].isos[i]+'_'+prec[r]+'> ';
                      a+=tsl[no_tsl(prec[r])].isos[i]+' = ';
                      a+=tsl[utsl[s]].isos[i]+' : {%'+cfgname+'} ';            
                      a+='safediv( '+b+','+v+');';
                      writeln(e,a);
                      end;
{$ENDIF}
                   end;

            end
         else
             // spec is not produced in the mech
             if (i=1) then writeln(e,'{ '+spec+' is not produced in this mech }');

       end;   
     
     writeln(e);
     end;

{$IFDEF SPUNI}
a:='#INLINE F90_GLOBAL'+_LF;
a+='  ! RCs for reaction rates'+_LF;
a+='    REAL(dp) :: ';
for i:=1 to _utsl do
    for j:=2 to _isos do
        a+='RC'+clsname[j]+'_'+tsl[utsl[i]].spec+', ';
setlength(a,length(a)-2);
a+=_LF;
a+='#ENDINLINE'+_LF;
writeln(e,wrap(a,8,6));

// ratio calculation code
a:='#INLINE F90_RATES'+_LF;
a+='  SUBROUTINE UPDATE_RATIOS (Y)'+_LF;
a+='    IMPLICIT NONE'+_LF;
a+='    REAL(dp), INTENT(INOUT) :: Y(NVAR)'+_LF;
a+='    REAL(dp), PARAMETER     :: SAFE = 0.0_dp'+_LF;
a+='    REAL(dp)                :: FRAC'+_LF;
a+='  ! calculation of RCs'+_LF;
for s:=1 to _utsl do
    for j:=2 to _isos do
        a+='    Y('+substr(sisyntax,'@','RC'+clsname[j]+'_'+tsl[utsl[s]].spec)+') = Y('+substr(sisyntax,'@',tsl[utsl[s]].isos[j])+')'+_LF;
a+='  END SUBROUTINE UPDATE_RATIOS'+_LF;
a+='#ENDINLINE'+_LF;
writeln(e,a);
{$ENDIF}

{$IFDEF DONTWANNA}
// another block for conc control variant
a:='#INLINE F90_RATES'+_LF;
a+=_LF;
a+='  SUBROUTINE INIT_CONC'+_LF;
a+=_LF;
a+='    IMPLICIT NONE'+_LF;
a+=_LF;
a+='  ! initializing totals'+_LF;
a+=_LF;
for s:=1 to _utsl do
    begin
    // total composition
    a+='    C('+substr(sisyntax,'@','A'+tsl[utsl[s]].spec)+') = ';
    for j:=1 to _isos do
        a+='C('+substr(sisyntax,'@',tsl[utsl[s]].isos[j])+') + ';
    setlength(a,length(a)-2);
    a+=_LF;
    end;
a+=_LF;
a+='  END SUBROUTINE INIT_CONC'+_LF;
a+=_LF;

a+='  SUBROUTINE UPDATE_RCONC (Y)'+_LF;
a+=_LF;
a+='    IMPLICIT NONE'+_LF;
a+=_LF;
a+='    REAL(dp), INTENT(INOUT) :: Y(NVAR)'+_LF;
a+='    REAL(dp), PARAMETER     :: SAFE = 0.0_dp'+_LF;
a+='    REAL(dp)                :: FRAC, TOT'+_LF;
a+=_LF;
a+='  ! scaling classes to totals'+_LF;
a+=_LF;
for s:=1 to _utsl do
    begin
    a+='    TOT = ';
    for j:=1 to _isos do
        a+='Y('+substr(sisyntax,'@',tsl[utsl[s]].isos[j])+') + ';
    setlength(a,length(a)-2);
    a+=_LF;
    a+='    IF ( TOT .GT. SAFE ) THEN'+_LF;
    // scaling      
    for j:=1 to _isos do
        a+='      Y('+substr(sisyntax,'@',tsl[utsl[s]].isos[j])+') = Y('+substr(sisyntax,'@',tsl[utsl[s]].isos[j])+') / TOT * '+
                 'Y('+substr(sisyntax,'@','A'+tsl[utsl[s]].spec)+')'+_LF;
    a+='    ELSE'+_LF;
    for j:=1 to _isos do
        a+='      Y('+substr(sisyntax,'@',tsl[utsl[s]].isos[j])+') = 0.0_dp'+_LF;
    a+='    ENDIF'+_LF;
    a+=_LF;
    end;

a+='  END SUBROUTINE UPDATE_RCONC'+_LF;
a+=_LF;
a+=_LF+'#ENDINLINE'+_LF;
writeln(e,a);
{$ENDIF}

{$IFNDEF OPT_FINAL}
// block for weighting concentration variables
a:='#INLINE F90_GLOBAL'+_LF;
a+='  ! weighting concentrations declaration'+_LF;
a+='    REAL(dp) :: ';
for i:=1 to _utsl do
    a+='WEC_'+tsl[utsl[i]].spec+', ';
setlength(a,length(a)-2);
a+=_LF;
a+='#ENDINLINE'+_LF;
writeln(e,wrap(a,8,6));
// another block for conc control variant
a:='#INLINE F90_RCONST'+_LF;
a+='  ! calculation of weights'+_LF;
for s:=1 to _utsl do
    begin
    // total composition
    a+='    WEC_'+tsl[utsl[s]].spec+' = ';
    for j:=1 to _isos do
        a+='C('+substr(sisyntax,'@',tsl[utsl[s]].isos[j])+') + ';
    setlength(a,length(a)-2);
    a+=_LF;
    end;
a+='#ENDINLINE'+_LF;
writeln(e,a);
{$ENDIF}

close(e);

end;

// -----------------------------------------------------------------

// TAG@MECCA integration species list
procedure imtag_produce_spcINT_file(fname : string);

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

write('produce_spcINT_file(',fname,'): ');

assign(f,fname);
rewrite(f);

writeln(f);
writeln(f,'{-------------- [imtag] ------------------------------------------------------}');
writeln(f);
writeln(f,'{ [Gromov, MPI-C] =',datetimetostr(now),'= }');
writeln(f);

writeln(f,'{- new atoms -----------------------------------------------------------------}');
writeln(f);
writeln(f,'#INCLUDE atoms');
writeln(f);
writeln(f,'#ATOMS');
writeln(f);
for i:=1 to _isos do
    writeln(f,'  ',isoatom+clsname[i],'     { mass ',isomass[i]:0:7,' of element ',isoatom,' };');
writeln(f);

writeln(f,'{- species -------------------------------------------------------------------}');
writeln(f);

a:='';

writeln(f,'#DEFVAR');
writeln(f);
writeln(f,'{ isotpologue species }');
writeln(f);
for i:=1 to _utsl do
    with tsl[utsl[i]] do
         begin
         
         for j:=1 to _isos do
             safeaddspec(isos[j],spc[nspc].icomp[j]+'; { '+spec+' '+clsname[j]+isoatom+' isotopologue }');
         writeln(f);
         
         end;

{$IFDEF SPUNI}
writeln(f,'{ fraction species }');
writeln(f);
for i:=1 to _utsl do
    with tsl[utsl[i]] do
         begin
         
         for j:=2 to _isos do
             safeaddspec('RC'+clsname[j]+'_'+tsl[utsl[i]].spec,'IGNORE; { '+spec+' '+clsname[j]+isoatom+' RC product }');
//         writeln(f);
//         safeaddspec('INV_'+tsl[utsl[i]].spec,'IGNORE; { '+spec+' inverted }');
         end;
{$ENDIF}

{$IFDEF NOTNOW}
writeln(f,'{ species abundances }');
writeln(f);
for i:=1 to _utsl do
    with tsl[utsl[i]] do
         safeaddspec('A'+spec,'IGNORE; { '+spec+' '+clsname[j]+isoatom+' abundance }');
writeln(f);
{$ENDIF}

writeln(f,'  UNITY = IGNORE; { unity dummy species }');
writeln(f,'  SINK  = IGNORE; { sink dummy species }');
writeln(f);

{$IFDEF ADD_DUMIND}
writeln(f,'{- indices of species not tagged but still present in the TSL -----------------}');
writeln(f);

for i:=1 to _tsl do
    if not(is_usedspec(tsl[i].spec)) then
       with tsl[i] do
            begin
         
            for j:=1 to _isos do
                safeaddspec(isos[j],'IGNORE; { dummy spec }');
            writeln(f);

            end;
{$ENDIF}

writeln(f,'{-------------- [imtag] - end ------------------------------------------------}');
writeln(f);

close(f);

if (declspecs<>'') then write('(warning, declined as already existing species: ',declspecs,') ');

writeln('done');

end;

// -----------------------------------------------------------------------------

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
   writeln('usage: imtag.exe <scpfile> <eqnfile> <tagging configurations list> ');
   writeln('   ex:            mecca.spc gas.eqn   tagXX.cfg ');
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
    imcom_check_eqn_dupes(eqnfname,'Dummy');
    imtag_produce_spcPTs_file(spcfname);    // additional species list

    // prepare the molecules flow calculation code for current configuration
    prepare_flow_jac_calc(data);
    // update template replacements
    imcom_update_reps;
    imcom_reps[imtag_QCFLDIR,2]:=inttostr(flow_conf_ecnt); // <-- # of curr. conf. flow dirs.
    
{$IFDEF CRT_INTEQN}
    // creating "folded system" integration eqn/spc for TAG@MECCA
    imtag_produce_eqnINT_file(data, eqnfname_conf);
    imcom_check_eqn_dupes(eqnfname_conf,'UNITY');
    imtag_produce_spcINT_file(spcfname_conf);
{$ENDIF}

    // parse configuration F90 code formers
    for i:=1 to _form_conf do
        imtag_parse_code(data,form_conf[i,1],form_conf[i,2]);

    // tracer definition file
    imcom_make_addtracdef('gas.tex',tracdef);

{$IFDEF MAKE_DOTS}
    // produce configuration dot files for graphviz
    //   species names should be sparated with spaces
    if (isoatom='C') then
       imdot_produce_dot_files(data,cmodel+'_'+tagname,'CH4 C5H8','CO')
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
    imtag_parse_code(data,form_intr[i,1],form_intr[i,2]);

writeln;
writeln('[imtag]: done');
writeln;

//writeln('ALLFLOW: ');
//writeln(wrap(imtag_make_flow_calc(flow_intr_calc,'A($)'),8,8));

//writeln('DIRS: ',flow_intr_dirs);

//writeln('confdirs: ',wrap(imtag_make_flow_dirs(flow_conf_dirs,' # = $'),8,8));
//writeln('confdirs 2: ',wrap(imtag_make_flow_dirs(flow_conf_dirs,' #'),8,8));

end.
