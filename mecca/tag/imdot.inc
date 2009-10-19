// = imdot.inc =======================================================
// Include file for imtag for production of .dot file for graphviz
//
// Gromov, MPIC, 2007
// ===================================================================

function rem_specchar(s : ansistring) : ansistring;
begin
// avoiding control characters (like \n, \t, etc) by replacing \ to /
while (pos('\',s)>0) do
      begin
      insert('/',s,pos('\',s));
      delete(s,pos('\',s),1);
      end;
rem_specchar:=s;
end;

function rem_spaces(s : ansistring) : ansistring;
begin
// removing speces from string
while (pos(' ',s)>0) do
      delete(s,pos(' ',s),1);
rem_spaces:=s;
end;

// unique colors for unique reactions
var eqs_col : array[1..max_eqs] of string;

// species ranks used for species-plot
var rank_spec : array[1..max_tsl] of word; // species rank
var prod_sink : array[1..max_tsl,1..2] of word; // # of species producing / sink to
const rank_maxq = 50;		   // max reactions in the column

const rcm : array[0..10] of string =	   // reactions colomap
    ('white',             // default (or unknown)
     'firebrick',         // oxygen, G1-G2
     'dimgray',           // nitrates, G3
     'dodgerblue3',       // 1C carbons, G41
     'khaki4',     // higher carbons, rest of G4
     'green4',            // chlorine, G6
     'lightsalmon4',      // bromine, G7
     'blueviolet',        // iod, G8
     'yellow3',           // sulphur, G9
     'darkseagreen4'{'thistle4'},           // PSCs           exch nit & PSC
     'orange2');          // photolysis, J

procedure make_eqs_col;
var r : word;
begin

// adding legend to eqs... hm

for r:=1 to _eqs do
//    eqs_col[r]:=floattostrf(0.5-random*0.5,fffixed,7,3)+','+
  //	      floattostrf(1-random*0.3,fffixed,7,3)+','+
    //	      floattostrf(1-random*0.5,fffixed,7,3);
//    eqs_col[r]:=floattostrf(1.0-0.5*(r/_eqs),fffixed,7,3)+' '+
  //	      floattostrf(1.0-(r/_eqs),fffixed,7,3)+' '+
    //	      floattostrf(0.5+0.5*(r/_eqs),fffixed,7,3);
//    eqs_col[r]:=floattostrf((0.438+(r/_eqs)*(0.75-0.438)),fffixed,7,3)+' '+
  //	      floattostrf((0.125+(r/_eqs)*(0.75-0.125)),fffixed,7,3)+' '+
    //	      floattostrf((0.711*(1-(r/_eqs))),fffixed,7,3);
//    eqs_col[r]:=floattostrf( r/_eqs ,fffixed,7,3)+' '+       // hue
  //	      floattostrf( 0.75    ,fffixed,7,3)+' '+       // sat
    //	      floattostrf( 0.75    ,fffixed,7,3);   // lig
    begin

    eqs_col[r]:=rcm[0];

    if (pos('J',eqs[r].abbr)>0) then     // photolysis
       eqs_col[r]:=rcm[10]
    else
    if (pos('PSC',eqs[r].abbr)>0) then   // PSCs
       eqs_col[r]:=rcm[9]
    else
    if (pos('G',eqs[r].abbr)>0) then    // gas-phase...
       begin

       if ((pos('G1',eqs[r].abbr)>0) or
      (pos('G2',eqs[r].abbr)>0)) then eqs_col[r]:=rcm[1];  // oxygen
       if (pos('G3',eqs[r].abbr)>0) then eqs_col[r]:=rcm[2];   // nitrates
       if (pos('G41',eqs[r].abbr)>0) then eqs_col[r]:=rcm[3]   // 1C carbons
       else
       if (pos('G4',eqs[r].abbr)>0) then eqs_col[r]:=rcm[4]; // higher carbons
       if (pos('G6',eqs[r].abbr)>0) then eqs_col[r]:=rcm[5];     // chlorine, green
       if (pos('G7',eqs[r].abbr)>0) then eqs_col[r]:=rcm[6];     // bromine, brown
       if (pos('G8',eqs[r].abbr)>0) then eqs_col[r]:=rcm[7];     // iod, violet
       if (pos('G9',eqs[r].abbr)>0) then eqs_col[r]:=rcm[8];     // sulphur, yellow

       end
    end;
end;

type tscm = array[1..6,1..3] of string;    // species colormap

	   // line	// fill    // font
const scm1 : tscm =
	(('black',      'lightgoldenrod',  'black'),    // from
	 ('black',      'rosybrown1',      'black'),    // to
	 ('black',      'darkolivegreen3', 'black'),    // bcs
	 ('black',      'lightsteelblue2', 'black'),    // interm.
	 ('black',      'red1',            'white'),    // def.node
	 ('white', '50', '30'));  // bg, node fsize, edge fsize
      scm2 : tscm =
	(('orange',     'black',           'orange'),
	 ('firebrick1', 'black',           'firebrick1'),
	 ('green3',     'black',           'green3'),
	 ('ivory2',     'black',           'ivory2'),
	 ('gray',       'red',             'white' ),   // def.node
	 ('black', '50', '40'));  // bg, node fsize, edge fsize
      scm3 : tscm =
	(('orange4'{black},      'orange',          'black'),
	 ('darkolivegreen'{black}, 'olivedrab'{firebrick1},'white'{black}),
	 ('black',      'lightgoldenrod4', 'black'),
	 ('black',      'lemonchiffon3',   'black'),
	 ('gray',       'red',             'white' ),   // def.node
	 ('black', '50', '40'));  // bg, node fsize, edge fsize

procedure imdot_produce_dot_files(d : ttagset; fname : string; _from, _to : string);

var dotmech, dotspec, dotcomm : text;     // .dot files instances
    p, s, r, cp, cs, rank : word;
    a, prl, sign, kiestar, otheduc : ansistring;
    scm : tscm;                       // colorscheme
    rels : array[1..max_tsl,1..max_tsl+1] of string; // relations for common-relations plot
    is_dest : boolean;	              // "destruction-only" flag
    tmp : textfile;                   // template file

function make_spec_decl(spec, specid : string) : ansistring;
var s : ansistring;
begin
s:='  '+specid+' [ label="'+spec+'", ';
if (pos(spec+' ',_from)>0) then
   s:=s+' shape=diamond, color='+scm[1,1]+', fillcolor='+scm[1,2]+' fontcolor='+scm[1,3]
else
    if (pos(spec+' ',_to)>0) then
       s:=s+' shape=invtriangle, color='+scm[2,1]+', fillcolor='+scm[2,2]+', fontcolor='+scm[2,3]
    else
    if (in_bcs(spec)) then
       if (is_usedspec(spec)) then
	  s:=s+' color='+scm[3,1]+', fillcolor='+scm[3,2]+', fontcolor='+scm[3,3]
       else
	   s:=s+' color='+scm[3,1]+', fillcolor='+scm[3,3]+', fontcolor='+scm[3,2]
    else
	if (in_tsl(spec)) then
	   s:=s+' color='+scm[4,1]+', fillcolor='+scm[4,2]+', fontcolor='+scm[4,3];
s:=s+' ];';
make_spec_decl:=s;
end;


begin

write('produce_dot_file(',fname,',',_from,' > ',_to,'): ');

_from+=' '; _to+=' ';

imcom_check_files_exist(['imtag.dot']);

randomize;

make_eqs_col; // prepare colormap for equations

// select colorscheme
scm:=scm3;   // cs1-3

assign(dotmech,fname+'.dotmech.dot'); rewrite(dotmech);
assign(dotspec,fname+'.dotspec.dot'); rewrite(dotspec);
assign(dotcomm,fname+'.dotcomm.dot'); rewrite(dotcomm);

a:='digraph '+tagname+' {'+_LF;
a:=a+_LF;

// copying a header from template
assign(tmp,'imtag.dot'); reset(tmp);
while not(eof(tmp)) do
      begin
      readln(tmp,prl);
      a:=a+prl+_LF;
      end;
close(tmp);

a:=a+'  label = "IMTAG // ATOM: <'+isoatom+'> ='+datetimetostr(now)+'= [Gromov, MPI-C]\n"+'+_LF+
     '          "TSL: '+rem_specchar(paramstr(2))+' ['+inttostr(_utsl)+'/'+inttostr(_tsl)+'] '+
                'EQN: '+rem_specchar(paramstr(1))+' ['+inttostr(nooftagreac)+'/'+inttostr(_eqs)+']\n"+'+_LF+
     '          "MECH:'+wraptext(rem_specchar(rem_spaces(eqnfile[1].line)),'\n',['=',' ',',','(',')','!'], 40)+'"'+_LF+
     '          "='+datetimetostr(now)+'= [Gromov, MPI-C]"'+_LF;
a:=a+_LF;
a:=a+'  bgcolor = '+scm[6,1]+';'+_LF;
a:=a+_LF;
a:=a+'  node [color='+scm[5,1]+', fillcolor='+scm[5,2]+', fontcolor='+scm[5,3]+', style="bold, filled" ];'+_LF;
a:=a+'  edge [color='+scm[5,1]+', fontcolor='+scm[5,3]+'];'+_LF;
a:=a+_LF;

// legend (based on MECCA selection) ------------------------------------------
a:=a+'  // legend'+_LF;

a:=a+'  intermediate [color='+scm[4,1]+', fillcolor='+scm[4,2]+', fontcolor='+scm[4,3]+'];'+_LF;
a:=a+'  source [color='+scm[3,1]+', fillcolor='+scm[3,2]+', fontcolor='+scm[3,3]+'];'+_LF;
a:=a+'  AVEx [label = "AVE"];'+_LF;
a:=a+'  AVEx -> AVEx [label = "pool average", fontcolor=gray ];'+_LF;
a:=a+'  LOSSx [label = "LOSS"];'+_LF;
a:=a+'  LOSSx -> LOSSx [label = "destruction", fontcolor=gray ];'+_LF+_LF;

writeln(dotcomm,a);

a:=a+'  G12xx -> G12xx [label="(oxygen)", color='+rcm[1]+', fontcolor='+rcm[1]+'];'+_LF;
a:=a+'  G3xxx -> G3xxx [label="(nitrates)", color='+rcm[2]+', fontcolor='+rcm[2]+'];'+_LF;
a:=a+'  G41xx -> G41xx [label="(1C carbons)", color='+rcm[3]+', fontcolor='+rcm[3]+'];'+_LF;
a:=a+'  G4xxx -> G4xxx [label="(higher carbons)", color='+rcm[4]+', fontcolor='+rcm[4]+'];'+_LF;
a:=a+'  G6xxx -> G6xxx [label="(chlorine)", color='+rcm[5]+', fontcolor='+rcm[5]+'];'+_LF;
a:=a+'  G7xxx -> G7xxx [label="(bromine)", color='+rcm[6]+', fontcolor='+rcm[6]+'];'+_LF;
a:=a+'  G8xxx -> G8xxx [label="(iod)", color='+rcm[7]+', fontcolor='+rcm[7]+'];'+_LF;
a:=a+'  G9xxx -> G9xxx [label="(sulphur)", color='+rcm[8]+', fontcolor='+rcm[8]+'];'+_LF;
a:=a+'  PSCxxx -> PSCxxx [label="(PSCs)", color='+rcm[9]+', fontcolor='+rcm[9]+'];'+_LF;
a:=a+'  Jxxxx -> Jxxxx [label="Photolysis", color='+rcm[10]+', fontcolor='+rcm[10]+'];'+_LF;

a:=a+'  G12xx [label="G1xxx, G2xxx", color='+rcm[1]+', fontcolor='+rcm[1]+', fillcolor='+scm[6,1]+', style="bold" ];'+_LF;
a:=a+'  G3xxx [color='+rcm[2]+', fontcolor='+rcm[2]+', fillcolor='+scm[6,1]+', style="bold" ];'+_LF;
a:=a+'  G41xx [color='+rcm[3]+', fontcolor='+rcm[3]+', fillcolor='+scm[6,1]+', style="bold" ];'+_LF;
a:=a+'  G4xxx [color='+rcm[4]+', fontcolor='+rcm[4]+', fillcolor='+scm[6,1]+', style="bold" ];'+_LF;
a:=a+'  G6xxx [color='+rcm[5]+', fontcolor='+rcm[5]+', fillcolor='+scm[6,1]+', style="bold" ];'+_LF;
a:=a+'  G7xxx [color='+rcm[6]+', fontcolor='+rcm[6]+', fillcolor='+scm[6,1]+', style="bold" ];'+_LF;
a:=a+'  G8xxx [color='+rcm[7]+', fontcolor='+rcm[7]+', fillcolor='+scm[6,1]+', style="bold" ];'+_LF;
a:=a+'  G9xxx [color='+rcm[8]+', fontcolor='+rcm[8]+', fillcolor='+scm[6,1]+', style="bold" ];'+_LF;
a:=a+'  PSCxxx [color='+rcm[9]+', fontcolor='+rcm[9]+', fillcolor='+scm[6,1]+', style="bold" ];'+_LF;
a:=a+'  Jxxxx [color='+rcm[10]+', fontcolor='+rcm[10]+', fillcolor='+scm[6,1]+', style="bold" ];'+_LF;

a:=a+'  { rank=max; G12xx G3xxx G41xx G4xxx G6xxx G7xxx G8xxx G9xxx Jxxxx AVEx LOSSx intermediate source }'+_LF;
a:=a+_LF;

writeln(dotmech,a);
writeln(dotspec,a);

// now creating common plot (dotmech) -----------------------------------------

// species
for s:=1 to _utsl do
    with d[s] do
	 begin
	 writeln(dotmech,make_spec_decl(spec,spec));
	 writeln(dotcomm,make_spec_decl(spec,spec));
	 end;

prl:='';    // passed reactions list

// by species
for s:=1 to _utsl do
    with d[s] do
     begin

     // production reactions
     for r:=1 to _prod do
     begin
     sign:=':'+prec[r]+'>'+spec+'_'+eqs[prod[r]].abbr+'|';
     if (pos(sign,prl)=0) then
	begin

	if (is_kie(eqs[prod[r]].abbr)) then kiestar:=' *' else kiestar:='';

	otheduc:='';
	if (eqs[prod[r]].educ[1]<>prec[r]) then otheduc:=' +'+eqs[prod[r]].educ[1];
	if ((eqs[prod[r]].educ[2]<>prec[r]) and
	(eqs[prod[r]].educ[2]<>'')) then otheduc:=' +'+eqs[prod[r]].educ[2];

	writeln(dotmech,'  ',prec[r],' -> ',spec,' ',
	    '[',
	    'label="',eqs[prod[r]].abbr,kiestar,otheduc,
	    //' (#',inttostr(prod[r]),') ',
	    //'[', {prec[r],'>',spec,}']',
	    '", ',
	    'color=',eqs_col[prod[r]],', ',
	    'fontcolor=',eqs_col[prod[r]],' ',
	    '];');
	prl:=prl+sign;
	end;
     end;

     // sink reactions
     for r:=1 to _sink do
     begin
     is_dest:=true;

     if (is_kie(eqs[sink[r]].abbr)) then kiestar:=' *' else kiestar:='';

     otheduc:='';
     if (eqs[sink[r]].educ[1]<>spec) then otheduc:=' +'+eqs[sink[r]].educ[1];
     if ((eqs[sink[r]].educ[2]<>spec) and
	 (eqs[sink[r]].educ[2]<>'')) then otheduc:=' +'+eqs[sink[r]].educ[2];

     for p:=1 to eqs[sink[r]]._prod do
	 begin

	 sign:=':'+spec+'>'+eqs[sink[r]].prod[p]+'_'+eqs[sink[r]].abbr+'|';
	 if (in_tsl(eqs[sink[r]].prod[p])) then
	begin
	is_dest:=false;
	if (pos(sign,prl)=0) then
	   begin
	   writeln(dotmech,'  ',spec,' -> ',eqs[sink[r]].prod[p],' ',
		   '[',
		   'label="',eqs[sink[r]].abbr,kiestar,otheduc,
		   //' (#',inttostr(prod[r]),') ',
		   //'[', {prec[r],'>',spec,}']',
		   '", ',
		   'color=',eqs_col[sink[r]],', ',
		   'fontcolor=',eqs_col[sink[r]],' ',
		   '];');
	   prl:=prl+sign;
	   end;
	end;
	 end;
     if (is_dest) then
	writeln(dotmech,'  ',spec,' -> ','LOSS',' ',
	    '[',
	    'label="',eqs[sink[r]].abbr,kiestar,otheduc,
	    //' (#',inttostr(prod[r]),') ',
	    //'[', {prec[r],'>',spec,}']',
	    '", ','arrowhead=odot, ',
	    'color=',eqs_col[sink[r]],', ',
	    'fontcolor=',eqs_col[sink[r]],' ',
	    '];');
     end;
     end;
writeln(dotmech);

// now creating separated-species plot (dotspec) ------------------------------

fillchar(prod_sink,sizeof(prod_sink),0);

// by species
for s:=1 to _utsl do
    with d[s] do
         begin

         sign:='_'+inttostr(s);
         writeln(dotspec,make_spec_decl(spec,spec+sign));
         prl:='';     // using as passed-species list

         // production reactions
         for r:=1 to _prod do
             begin

	     if (pos(('>'+prec[r]+'<'),prl)=0) then
	        begin
		writeln(dotspec,make_spec_decl(prec[r],prec[r]+sign));
		prl:=prl+':'+'>'+prec[r]+'<';
		end;

             if (is_kie(eqs[prod[r]].abbr)) then kiestar:=' *' else kiestar:='';

	     otheduc:='';
             if (eqs[prod[r]].educ[1]<>prec[r]) then otheduc:=' +'+eqs[prod[r]].educ[1];
	     if ((eqs[prod[r]].educ[2]<>prec[r]) and
	         (eqs[prod[r]].educ[2]<>'')) then otheduc:=' +'+eqs[prod[r]].educ[2];

             writeln(dotspec,'  ',prec[r]+sign,' -> ',spec+sign,' ',
	                     '[',
	                     'label="',eqs[prod[r]].abbr,kiestar,otheduc,
	                     //' (#',inttostr(prod[r]),') ',
	                     //'[', {prec[r],'>',spec,}']',
	                     '", ',
	                     'color=',eqs_col[prod[r]],', ',
	                     'fontcolor=',eqs_col[prod[r]],' ',
	                     '];');
             inc(prod_sink[s,1]);
	     end;

         // sink reactions
         for r:=1 to _sink do
             begin
             is_dest:=true;
    
             if (is_kie(eqs[sink[r]].abbr)) then kiestar:=' *' else kiestar:='';
    
    	     otheduc:='';
    	     if (eqs[sink[r]].educ[1]<>spec) then otheduc:=' +'+eqs[sink[r]].educ[1];
    	     if ((eqs[sink[r]].educ[2]<>spec) and
    	         (eqs[sink[r]].educ[2]<>'')) then otheduc:=' +'+eqs[sink[r]].educ[2];
    
    	     for p:=1 to eqs[sink[r]]._prod do
                 if (in_tsl(eqs[sink[r]].prod[p])) then
                    begin
                    is_dest:=false;
    
    	            if (pos(('>'+eqs[sink[r]].prod[p]+'<'),prl)=0) then
    	               begin
    	               writeln(dotspec,make_spec_decl(eqs[sink[r]].prod[p],
    	                               eqs[sink[r]].prod[p]+sign));
    		                       prl:=prl+':'+'>'+eqs[sink[r]].prod[p]+'<';
                       end;
    
    		    writeln(dotspec,'  ',spec+sign,' -> ',eqs[sink[r]].prod[p]+sign,' ',
                                    '[',
    			            'label="',eqs[sink[r]].abbr,kiestar,otheduc,
    			            //' (#',inttostr(prod[r]),') ',
    			            //'[', {prec[r],'>',spec,}']',
    			            '", ',
    			            'color=',eqs_col[sink[r]],', ',
    			            'fontcolor=',eqs_col[sink[r]],' ',
    			            '];');
    		    inc(prod_sink[s,2]);
    		    end;
    
    		    if (is_dest) then
    		       begin
    		       writeln(dotspec,make_spec_decl('LOSS','LOSS'+sign));
    		       writeln(dotspec,'  ',spec+sign,' -> ','LOSS'+sign,' ',
    		                       '[',
    			               'label="',eqs[sink[r]].abbr,kiestar,otheduc,
    			               //' (#',inttostr(prod[r]),') ',
    			               //'[', {prec[r],'>',spec,}']',
    			               '", ','arrowhead=odot, ',
    			               'color=',eqs_col[sink[r]],', ',
    			               'fontcolor=',eqs_col[sink[r]],' ',
    			               '];');
    		       inc(prod_sink[s,2]);
    		       end;
    
    		    end;
           
	     writeln(dotspec);
             end;

// sorting layout
cp:=0; cs:=0; rank:=1;	 // production, sink and rank counters
fillchar(rank_spec,sizeof(rank_spec),0);
while (true) do
      begin

      r:=0;
      for p:=1 to _utsl do
      if (rank_spec[p]=0) then	  // choosing not rank-assigned spec left
	 begin r:=p; break; end;
      if (r=0) then break;	  // no specs left, finishing

      inc(cp,prod_sink[r,1]); inc(cs,prod_sink[r,2]);
      rank_spec[r]:=rank;

      for p:=1 to _utsl do
      if (rank_spec[p]=0) then	  // choosing not rank-assigned spec left
	 if ((cs+d[p]._sink)<rank_maxq) and
	((cp+d[p]._prod)<rank_maxq) then
	begin
	inc(cp,prod_sink[p,1]); inc(cs,prod_sink[p,2]);
	rank_spec[p]:=rank;
	end;

      if (cs>rank_maxq) or (cp>rank_maxq) then
     begin cp:=0; cs:=0; inc(rank); end;

      end;

a:=''; sign:='';
for r:=1 to rank do
    begin

    sign:=sign+'  { rank=same; P'+inttostr(r*3);
    for s:=1 to _utsl do
    if rank_spec[s]=r then sign:=sign+' '+tsl[utsl[s]].spec+'_'+inttostr(s);
    sign:=sign+' }'+_LF;

    a:=a+' P'+inttostr(r*3-1)+' -> P'+inttostr(r*3)+' -> P'+inttostr(r*3+1)+' ->';
    end;
setlength(a,length(a)-2);
writeln(dotspec,'  { node [ style=invis ]; edge [ style=invis ]; ',a,'; }');
writeln(dotspec,sign);

// now creating common-relations plot (dotcomm) -------------------------------

fillchar(rels,sizeof(rels),0);

// by species
for s:=1 to _utsl do
    with d[s] do
     begin

     // production reactions
     for r:=1 to _prod do
         if (pos(eqs[prod[r]].abbr+' ',rels[no_tsl(prec[r]),no_tsl(spec)])=0) then
            begin
	    if (is_kie(eqs[prod[r]].abbr)) then rels[no_tsl(prec[r]),no_tsl(spec)]+=' *';
	    rels[no_tsl(prec[r]),no_tsl(spec)]+=eqs[prod[r]].abbr+' '; // ' \n';
	    end;

     // sink reactions
     for r:=1 to _sink do
         begin
         is_dest:=true;

         if (is_kie(eqs[sink[r]].abbr)) then kiestar:='*' else kiestar:='';

         for p:=1 to eqs[sink[r]]._prod do
             if (in_tsl(eqs[sink[r]].prod[p])) then
	        begin
                is_dest:=false;
	     
                if (pos(eqs[sink[r]].abbr+' ',rels[no_tsl(spec),no_tsl(eqs[sink[r]].prod[p])])=0) then
	           rels[no_tsl(spec),no_tsl(eqs[sink[r]].prod[p])]+=kiestar+eqs[sink[r]].abbr+' '; // ' \n';
                end;

         if (is_dest) then
            if (pos(eqs[sink[r]].abbr+' ',rels[no_tsl(spec),_tsl+1])=0) then
               rels[no_tsl(spec),_tsl+1]+=kiestar+eqs[sink[r]].abbr+' '; // ' \n'
	 end;

     end;

// relations are done, making dot

// general links
for s:=1 to _tsl do
    for p:=1 to _tsl do
        if (rels[s,p]<>'') then
	   begin

           // wrapping the reactions list 
	   rels[s,p]:=wraptext(trim(rels[s,p]), '\n', _delims, 16);
	   // cutting last CR
	   setlength(rels[s,p],length(rels[s,p])-2);
	   
	   // info from -> to
	   rels[s,p]:=tsl[s].spec+'->'+tsl[p].spec+':\n'+rels[s,p];

           // random colour, H-S-L coding
	   a:=floattostrf(0.22-random*0.14,fffixed,7,3)+','+
  	      floattostrf(1.0-random*1.0,fffixed,7,3)+','+
    	      floattostrf(1.0-random*0.0,fffixed,7,3);
	   
	   writeln(dotcomm,'  ',tsl[s].spec,' -> ',tsl[p].spec,' ',
                           '[',
	                   'label="',rels[s,p],
                           '", ',
		           'color="',a,'", ',
		           'fontcolor="',a,'" ',
		           '];');
			   
	   end;
	   
// loss reactions	   
for s:=1 to _tsl do
    if (rels[s,_tsl+1]<>'') then
        writeln(dotcomm,'  ',tsl[s].spec,' -> LOSS ',
                           '[',
	                   'label="',rels[s,_tsl+1],
                           '", ',
		           'color=',rcm[1],', ',
		           'fontcolor=',rcm[1],' ',
		           '];');
writeln(dotcomm);


// finishing ------------------------------------------------------------------

writeln(dotmech,'}');
writeln(dotspec,'}');
writeln(dotcomm,'}');

close(dotmech);
close(dotspec);
close(dotcomm);

writeln('done');

end;
