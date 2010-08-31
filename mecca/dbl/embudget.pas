// in FPC >= 2.2.0

// = embudget.pas ====================================================
// (extended) mechanism budgeting tool
//
// requires: all is done through imcom.inc
//
// [S. Gromov, MPIC, 2009]
// ===================================================================

program embudget;

// - using imtag/dbl routines ----------------------------------------

{$DEFINE TAG}      // for compatibility with imcom

{$DEFINE IGNORE_NOSRC}   // ignoring missing sources when reading eqn (see imcom)

{$I imcom.inc}

// - types and system chars ------------------------------------------

const _undef : real = -1E-33;

var tsla : array[0..max_tsl] of record       // additional to TSL array with data
       _cats : integer;			     // # of budgeting categoriess
        ncat, ccat : array[1..max_eqs] of nstr;  // name and condition of each cat
        uprod, uloss : array[1..max_eqs] of boolean;
        end;

procedure embudget_read_config(spcfile, eqnfile, ininame : string);

// extract the next first parameter in the line (i.e. remove it)
function ext_next(var s : ansistring) : string;
var a : string;
begin
s:=trim(s); s:=s+' ';
a:=copy(s,1,pos(' ',s));
delete(s,1,length(a)-1);
a:=trim(a);
ext_next:=a;
end;

var i, j, k, l : integer;
    s, a, b : ansistring;
    r : real;

    ini : tinifile;
    info : tstringlist;

begin

writeln('reading info file: ',ininame);  // a ext
writeln;

// reading tagging info file
imcom_preprocess_ini(ininame);
ini:=tinifile.create('imcom.tmp');
info:=tstringlist.create;
info.clear;

cfgname:=ininame;

// ---------------------------------------------------------
// MECCA-specific params

// species' index syntax (opt., default is ind_@)
sisyntax:=extractword(1,ini.readstring('MECCA','sisyntax','ind_@'),_delims);

// PTs syntax (opt., default is PT@)
ptsyntax:=extractword(1,ini.readstring('MECCA','ptsyntax','PT@'),_delims);
ptlsyntax:=extractword(1,ini.readstring('MECCA','ptlsyntax','PTL@'),_delims);
ptpsyntax:=extractword(1,ini.readstring('MECCA','ptpsyntax','PTL@'),_delims);

// doubled reac. naming syntax (opt., default is D@)
drsyntax:=extractword(1,ini.readstring('MECCA','drsyntax','D@'),_delims);

// ---------------------------------------------------------
// reading species

writeln(' species: budgeting categories');

info.clear;
ini.readsection('SPC',info);
if (info.count<=0) then
   imcom_error('error: [SPC] section information is missing. stop.');

// initialising data
fillchar(tsl,sizeof(tsl),0); fillchar(tsla,sizeof(tsla),0);

for j:=0 to info.count-1 do
    if (info[j]<>'') then   // necessary condition to avoid empty keys of occasional trash in cfg
       begin

       // cheating imcom
       inc(_tsl);
       tsl[_tsl].spec:=extractword(1,info[j],_delims);
       tsl[_tsl].qatm:=strtointdef(extractword(2,info[j],_delims),1);

       s:=ini.readstring('SPC',info[j],'');
       if (pos('L',upcase(s))>0) then tsl[_tsl].iloss:=true;
       if (pos('P',upcase(s))>0) then tsl[_tsl].iprod:=true;
       end;

// ---------------------------------------------------------
// reading budgeting groups into ncat[_cats], conditions into ccat[_cats]

for i:=1 to _tsl do
 with tsl[i],tsla[i] do
    begin
    
    info.clear;
    ini.readsection(spec,info);
    if (info.count<=0) then
        imcom_error('error: species ['+spec+'] section information is missing. stop.');

    write(' ',spec,': ');

    tsla[i]._cats:=0;
    for j:=0 to info.count-1 do
        if (info[j]<>'') then   // necessary condition to avoid empty keys of occasional trash in cfg
           begin

           // cheating imcom further
           inc(_cats);
           ncat[_cats]:=info[j];
           ccat[_cats]:=ini.readstring(spec,info[j],'');
           
           write(ncat[_cats],'(',ccat[_cats],') ');
           end;
           
    writeln;
    end;

info.destroy;
ini.destroy;

writebreak;

// importing mecca species data using imcom
imcom_read_spc(spcfile);

// importing equations using imcom
imcom_read_eqs(eqnfile);

end;


// budgeting routine, produces eqn/spc file

procedure budgetit(spcname, eqnname : string);

// - budgetit body

var f : text;
    i, j, k, l, e, npt : integer;
    a, b, c, out : ansistring;

   _bs : integer; 
    bs : array[1..max_prod] of record
         spec : nstr;    // species to budget
         stoi : real;    // coefficient
         il : boolean;   // is loss?
         end;
         
begin

write('budgetit(',eqnname,',',spcname,',',cfgname,'): ');

out:='';  // output

for l:=1 to _eqnfile do
    if not((eqnfile[l].iseq) and (eqs[eqnfile[l].eqno].itag)) then
       out+=eqnfile[l].line+_LF         // just output the line if it is not an equation
    else
        with eqs[eqnfile[l].eqno] do
          begin
          c:=eqnfile[l].line;

          _bs:=0;

          // processing educts
          inc(_bs); bs[_bs].spec:=educ[1]; bs[_bs].stoi:=1.0; bs[_bs].il:=true;
          
          if (educ[2]<>'') then 
             begin

             if (educ[1]=educ[2]) then
                bs[_bs].stoi:=2
             else
                 begin
                 inc(_bs); bs[_bs].spec:=educ[2]; bs[_bs].stoi:=1.0; bs[_bs].il:=true;
                 end;
             end;

          // products
          for j:=1 to _prod do
              begin
              inc(_bs); bs[_bs].spec:=prod[j]; bs[_bs].stoi:=stoi[j]; bs[_bs].il:=false;
              end;

          // checking if we need to add PTs for species in bs[]
          a:='';
          for k:=1 to _bs do
              if in_tsl(bs[k].spec) then
                 with tsl[no_tsl(bs[k].spec)],tsla[no_tsl(bs[k].spec)] do  
                      begin

                      // loss PTs
                      if (iloss) and (bs[k].il) then
                         begin

                         if (abs(bs[k].stoi)<>1.0) then b:=floattostr(abs(bs[k].stoi))+' ' else b:='';
                         if not(bs[k].stoi<0.0) then b:=' + '+b else b:=' - '+b;
                         
                         // checking if r-n in a certain PT category
                         for e:=1 to _cats do
                          for i:=1 to wordcount(ccat[e],_delims) do
                             if (iswild(abbr, extractword(i,ccat[e],_delims), true)) then
                                begin
                                a+=b+substr(ptlsyntax,'@',ncat[e]+spec);   // ' + '
                                uloss[e]:=true;    // flag if used
                                end;
                         end;

                      // production PTs
                      if (iprod) and not(bs[k].il) then
                         begin

                         if (abs(bs[k].stoi)<>1.0) then b:=floattostr(abs(bs[k].stoi))+' ' else b:='';
                         if not(bs[k].stoi<0.0) then b:=' + '+b else b:=' - '+b;
                         
                         // checking if r-n in a certain PT category
                         for e:=1 to _cats do  
                          for i:=1 to wordcount(ccat[e],_delims) do
                             if (iswild(abbr, extractword(i,ccat[e],_delims), true)) then
                                begin
                                a+=b+substr(ptpsyntax,'@',ncat[e]+spec);   // ' + '
                                uprod[e]:=true;    // flag if sed
                                end;
                         end;
                      end;

          // inserting PTs before the reaction rate
          insert(' '+trim(a)+' ',c,pos(':',c));   // inserting in original EQ

          out+=c+_LF;
          end;

// some info
a:='';
a+='#INLINE F90_GLOBAL'+_LF;
a+='! -------------------------------------------------------------------------'+_LF;
a+='! current mechanism is budgeted by [embudget]'+_LF;
a+='! configuration: '+cfgname+_LF;
a+='! list of budgeted species/categories: '+_LF;
npt:=0;
for i:=1 to _tsl do
 with tsl[i],tsla[i] do
    begin
    a+='!  prod '+spec+' :';
    for j:=1 to _cats do
        if (uprod[j]) then
           begin
           a+=' '+ncat[j]; //+'('+ccat[j]+') ';
           inc(npt);
           end;
    a+=_LF;
    a+='!  loss '+spec+' :';
    for j:=1 to _cats do
        if (uloss[j]) then
           begin
           a+=' '+ncat[j]; //+'('+ccat[j]+') ';
           inc(npt);
           end;
    a+=_LF;
    end;        
a+='! # of added PTs: '+inttostr(npt)+_LF;
a+='! # of reactions in the selected mechanism: '+inttostr(_eqs)+_LF;
a+='! # of budgeted reactions: '+inttostr(nooftagreac)+_LF;
a+='! ='+datetimetostr(now)+'='+_LF;
a+='! --v-- further comes modified mechanism ----------------------------------'+_LF;
a+='#ENDINLINE'+_LF+_LF;

out:=a+out;

// flushing eqn
assign(f,eqnname);
rewrite(f);
write(f,out);
close(f);

// adding PTs to the SPC
assign(f,spcname);
rewrite(f);

// flushing original SPC
for i:=1 to _spcfile do
    writeln(f,spcfile[i].line);

writeln(f);
writeln(f,'{-------------- [embudget] ----------------------------------------------------}');
writeln(f);
writeln(f,'{ additional PTs list for budgeted mechanism based on: ',eqnname,' }');
writeln(f,'{ configuration: ',cfgname,' }');
writeln(f,'{ # of PTs added: ',npt,' }');
writeln(f);
writeln(f,'{ =',datetimetostr(now),'= }');
writeln(f);
writeln(f,'{- passive production tracers ------------------------------------------------}');
writeln(f);

for i:=1 to _tsl do
    with tsl[i],tsla[i] do
         begin
         writeln(f,'{--- ',spec,' ---}');
         for j:=1 to _cats do
             begin
             if (iloss) and (uloss[j]) then
                writeln(f,'  ',substr(ptlsyntax,'@',ncat[j]+spec),' = IGNORE; ',
                          '{ '+ccat[j]+' category loss passive tracer }');
             if (iprod) and (uprod[j]) then
                writeln(f,'  ',substr(ptpsyntax,'@',ncat[j]+spec),' = IGNORE; ',
                          '{ '+ccat[j]+' category production passive tracer }');
             end;
         writeln(f);
         end;

writeln(f,'{-------------- [embudget] - end ----------------------------------------------}');
writeln(f);

close(f);

writeln('done');

end;


// --- make_addtracdef --------------------------------------------------------
// creates additional tracers definition file to link with MESSy
procedure embudget_make_addtracdef(origTDname, addTDname : string; donew : boolean);

// this function substitutes a certain column value with another
// style is similar to tracdef.tex, i.e. 
// 'TRACER'  & col#2    & col#3  & etc.
// = column separators are '&' and '\'
function sub_column(src : string; colno : integer; _repl : string) : string;
var p, n, w : integer;
    s : string;
begin
s:=src;
// searching for a column and getting its width
p:=0; w:=0; n:=0;
while ((p<length(src)) and (n<colno)) do
      begin
      inc(p);                      // points to the end of col.
      inc(w);                      // curr. col width
      if (s[p] in ['&','\']) then
         begin
         inc(n);                   // passed column no.
         if (n<colno) then w:=0;   // resetting column width
         end;
      end;
if (n=colno) then                  // we found the col.!
   begin
   dec(w); dec(p,w);
   delete(s,p,w);                  // removing old cont., putting new
   insert(format('%-'+inttostr(w)+'s',[_repl]),s,p);
   end;
sub_column:=s;
end;

var f : text;
    tdfile : array[1..max_spc*2] of string;    // careful here with long strings in *.tex !!!
   _tdfile : integer;                          // # of lines in the file
    i, j, k : integer;
    otrac : nstr;
    s, ssave : string;

const rmolarmasscol = 17;                      // # of the column for the Rmolarmass
      mediumcol = 3;                           //    ------- " -------    media
      vinicol = 4;                             //    ------- " -------    vini
      totalcol = 24;                           // total # of the columns in tex
      off_col_beg = 6; off_col_end = 12;       // columns to put OFF value for totals

begin

write('embudget_make_addtracdef(',origTDname,',',addTDname,'): ');

// reading original file to get an example from
imcom_check_files_exist([origTDname]);
assign(f,origTDname); reset(f);
_tdfile:=0; fillchar(tdfile,sizeof(tdfile),0);
while not(eof(f)) do
      begin
      inc(_tdfile);
      readln(f,tdfile[_tdfile]);
      end;
close(f);

// opening output file handle
assign(f,addTDname); 
if (donew) then rewrite(f) else reset(f);

writeln(f,'% this file contains additional passive tracer definitions created by [embudget]');
writeln(f,'% configuration: ',cfgname);
writeln(f,'% created: ',datetimetostr(now));
writeln(f,'%');

// copying first 4 lines (supp. to be the table header)
for i:=1 to min(4,_tdfile) do
    writeln(f,tdfile[i]);

ssave:='';

// processing sequence: finding a spec, if it is used in the tagging, adding new tracers according to the classes
for i:=1 to _tdfile do
    if (pos('%',tdfile[i])=0) then         // skipping commented lines
       begin

       // getting original tracer name
       otrac:=extractword(1,tdfile[i],_delims);
       while (pos('''',otrac)>0) do
             delete(otrac,pos('''',otrac),1);
       otrac:=trim(otrac);

       if (in_tsl(otrac)) then
          begin

          // saving an example of a tracer def. line
          ssave:=tdfile[i];

          // headers for the tagging tracers
          s:='%--'+otrac+'-';
          while (length(s)<length(tdfile[i])) do s+='-';
          writeln(f,s);

          k:=no_tsl(otrac);
          with tsl[k],tsla[k] do
               for j:=1 to _cats do
                   begin
                   if (iloss) and (uloss[j]) then
                      begin
                      // tracer name
                      s:=sub_column(tdfile[i], 1, ''''+substr(ptlsyntax,'@',ncat[j]+spec)+'''');
                      // removing all initial data (except the medium)
                      for k:=vinicol to totalcol do
                          s:=sub_column(s, k, ' ');
                      // turning off all I_ parameters
                      for k:=off_col_beg to off_col_end do
                          s:=sub_column(s, k, ' OFF');
                      writeln(f,s);
                      end;
                      
                   if (iprod) and (uprod[j]) then
                      begin
                      // tracer name
                      s:=sub_column(tdfile[i], 1, ''''+substr(ptpsyntax,'@',ncat[j]+spec)+'''');
                      // removing all initial data (except the medium)
                      for k:=vinicol to totalcol do
                          s:=sub_column(s, k, ' ');
                      // turning off all I_ parameters
                      for k:=off_col_beg to off_col_end do
                          s:=sub_column(s, k, ' OFF');
                      writeln(f,s);
                      end;
                   end;
          end;

       end;

// last row lined (cosmetic)
s:='%--';
while (length(s)<length(tdfile[1])) do s+='-';
writeln(f,s);

close(f);

writeln('done');

end;


var nconf : integer;

const genname = 'messy_mecca_dbl_embudget';
      spcname = genname+'.spc';
      eqnname = genname+'.eqn';
      texname = genname+'.tex';

begin

if (paramcount<3) then
   begin
   writeln('>> MECCA kinetic meccanism extended budgeting');
   writeln('usage: ./embudget <spcfile> <eqnfile> <tracdeffile> <tagging configuration(s) list>');
   writeln('   ex:             zzz.spc   yyy.eqn   xxx.tex       tagXX.cfg [tagXY.cfg ...]');
   halt;
   end;

writeln('[embudget]');
writeln('=',datetimetostr(now),'=');
writebreak;

_conf:=paramcount-3;

for nconf:=1 to _conf do
    begin

    writeln;
    writeln('>> budgeting ',nconf,' of ',_conf,' configuration(s)...');
    writeln;

    if not(nconf>1) then
       begin
       embudget_read_config(paramstr(1), paramstr(2), paramstr(3+nconf));
       budgetit(spcname, eqnname);
       embudget_make_addtracdef(paramstr(3), texname, true);
       end
    else
        begin
        embudget_read_config(spcname, eqnname, paramstr(3+nconf));
        embudget_make_addtracdef(paramstr(3), texname, false);
        end;
    
    end;

writeln;
writeln('[embudget]: done');
writeln;

end.

