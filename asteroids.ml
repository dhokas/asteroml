
open Graphics;;

(* constantes et parametres *)

(* dimension fenetre graphique *)
let width = 1000.;;
let height = 600.;;
(* pour dessiner vaisseau *)
let cx,cy=width/.2.,height/.2.;;
let tx,ty = cx, 20.+.cy;;
let dx,dy = cx +.10.71, cy-.20.;;
let gx,gy=cx-.10.71,cy-.20.;;
(*pour dessiner flamme propulsion *)
let p1 = cx-.5.,cy-.0.;;
let p2 = cx-.2.,cy-.20.;;
let p3 = cx,cy-.10.;;
let p4 = cx+.2.,cy-.30.;;
let p5 = cx+.5.,cy-.0.;;

(*-------------*)
(*--- types ---*)
(*-------------*)

type couleur= R|V|B|J|TirBoss;;
type position=float*float;;
type taille = Petit|Moyen|Grand;;

type ast ={
couleur:couleur;
mutable positionA:position;
mutable vitesseA:float;
mutable taille:taille;
mutable directionA:float;
mutable detruitA:bool;
mutable colltmp:int
};;

type vaisseau = {
mutable positionV:position;
mutable directionV:float;
mutable vitesseV:float;
detruitV:bool;
mutable deceleration:float;
mutable chargementTir : int
};;

type tir = {
mutable tailleT:int;
mutable positionT:position; 
mutable directionT:float; 
mutable vitesseT:float;
mutable detruitT:bool;
mutable persistance:int
};;

type bonus = {
mutable positionB:position;
mutable effet:int;
mutable detruitB:bool
};;

type inventaire = { 
mutable amelioration :int;
mutable dureeAm : int;
mutable piece : int;
mutable vie : int;
mutable bouclier : bool
};;

type background = {
mutable etoiles:position list;
mutable newlvl : bool
};;

type jeu = {
mutable vulnerable: int;
mutable niveau: int;
mutable gameover : bool;
mutable score : int;
mutable scorefinal : int;
mutable highscore:int;
mutable inventaire : inventaire;
mutable pause : bool
}

type boss = {
mutable positionBoss : position;
mutable vieBoss : int;
mutable vieBossMax : float;
mutable chargementTirBoss : int;
mutable vitesseBoss : float;
mutable directionBoss : float;
mutable teleport : bool;
mutable ancienpos : position;
mutable lattel : float}

type etat = {
mutable tir:tir list;
mutable vaisseauV:vaisseau;
mutable ast:ast list;
mutable boss:boss list;
mutable background : background;
mutable jeu:jeu;
mutable bonus: bonus list;
mutable lpg : int;
mutable lpd : int;
mutable cycle : bool }
;;

type imageV = {
mutable t:position;
mutable d:position;
mutable g:position;
mutable p1:position; 
mutable p2:position;
mutable p3:position;
mutable p4:position;
mutable p5:position
};;

(*---------------------*)
(*--- constructeurs ---*)
(*---------------------*)

let new_vaisseau pos dir vit det cha= {positionV = pos ; directionV= dir; vitesseV = vit;detruitV=det;deceleration=0.;chargementTir=cha};;
let new_tir tai pos dir vit = {tailleT = 5+tai; positionT=pos; directionT=dir;vitesseT=vit;detruitT=false; persistance=0};;
let new_ast cou pos vit tai dir tmp ={couleur=cou;positionA=pos;vitesseA=vit;taille=tai;directionA=dir;detruitA=false;colltmp=tmp};;
let new_etat tirl vaiss astl boss background jeu= {tir=tirl; vaisseauV = vaiss ; ast=astl; boss=boss;background=background; jeu=jeu; bonus= []; lpg=0;lpd=0;cycle=true};;
let new_background etoiles = {etoiles = etoiles;newlvl=false};;
let new_jeu vul niv hig inv= {vulnerable=vul;niveau=niv;gameover=false; score=0; scorefinal=0; highscore=hig;inventaire =inv; pause = false};;
let new_inv ()= {amelioration=0; dureeAm = 0; piece=0;vie=2; bouclier =false};;
let new_bonus pos eff det= {positionB =pos; effet=eff;detruitB=det};;
let new_boss pos vie char vit dir = {positionBoss=pos; vieBoss=vie; vieBossMax=(float_of_int vie); chargementTirBoss = char; vitesseBoss = vit; directionBoss = dir; teleport = false; ancienpos = pos;lattel =0.};;

(*image vaisseau *)
let im = {t=tx,ty;d=dx,dy;g=gx,gy;p1=p1;p2=p2;p3=p3;p4=p4;p5=p5};;


(*-------------------*)
(*--- generateurs ---*)
(*-------------------*)


(* genere couleur ast *)
let genCouleur = function()->
	let i = Random.int 4 in
	match i with
	|0->R
	|1->V
	|2->B
	|3->J
        |_ -> J;;

(*genere vitesse *)
let genVitesse = function()-> 1. +. Random.float 16.;;

(*genere Taille *)
let genTaille = function() -> 
	let i = Random.int 3 in
	match i with
	|0->Petit
	|1->Moyen
	|2->Grand
        |_ -> Petit;;

(*genere direction *)
let genDirection = function()-> let pi =8.0 *. atan 1.0 in
	Random.float pi;;

(*genere position ast*)
let rec genPos = function() ->
	let x = Random.float 1001. in
	let y = Random.float 601. in
	if  (x > (1000./.3.) && x<(2000./.3.) && y>(600./.3.) && (y<(1200./.3.) || (x>width-.100. || x <100. || y>height-.100. || y<100.))) then
		genPos()
	else (x,y);;

(*genere position boss *)
let rec genPosBoss etat = function() ->
	let x = Random.float 1001. in
	let y = Random.float 601. in
	let xv,yv = etat.vaisseauV.positionV in
	if x > xv-.100. && x<xv +.100. && y> yv -.100. && y<yv+.100. && y>height-.100. && ( (x>width-.40. || x <40. || y>height-.40. || y<40.)) then
		genPos()
	else (x,y);;

(*genere ast *)
let rec genAstAux i= function() ->
	if i<=0 then []
	else let c =genCouleur() in
	     let p = genPos() in 
	     let v = genVitesse()in
	     let t = genTaille() in
	     let d = genDirection()in (match t with
		|Petit-> (new_ast c p v t d 0)::(genAstAux (i-1)())
		|Moyen ->(new_ast c p v t d 0)::(genAstAux (i-1)())
		|Grand ->(new_ast c p v t d 0)::(genAstAux (i-2)()));;
let genAst niv = function() ->
	let i = niv  + Random.int (0+niv) in
	genAstAux i ();;

(* genere etoiles *)
let rec genPosE = function() ->
	let x = Random.float 1001. in
	let y = Random.float 601. in
	if x > (1000.) && x<(0.) && y>(600.) && y<(0.) then
		genPosE()
	else (x,y);;


(*genere background*)
let rec genBackground i = function() ->
  match i with 
    |0 -> genPosE()::[]
    |_ -> genPosE()::genBackground (i-1)();;

(*genere bonus*)
let genBonus (x,y) = function() -> 
  let i = Random.int 20 in  new_bonus (x,y) i false;;

(*----------------------*)
(*--- initialisation ---*)
(*----------------------*)

let init_etat high  = let astl = genAst 1 () in
		   let background = (new_background (genBackground 125()) ) in
		   let inv = new_inv() in
		   let jeu = new_jeu 40 1 high inv in
		   new_etat []

                            (new_vaisseau (width/.2.,height/.2.) 0. 0. false 5)   
		     astl
		     []
		     background 
		     jeu;;

			 

(*---------------------------------------*)
(* acceleration/deceleration du vaisseau *)
(*---------------------------------------*)

let acceleration etat =  etat.vaisseauV.deceleration <- 0. ; if(etat.vaisseauV.vitesseV <=15.) then  etat.vaisseauV.vitesseV <- etat.vaisseauV.vitesseV +.5. else ();etat;;

let deceleration  etat = let vit = etat.vaisseauV.vitesseV  in
			  let dece =  etat.vaisseauV.deceleration in
			  if (dece >=0. && dece < 1.)
			  then ( etat.vaisseauV.deceleration <-(dece+.0.02); 
				 if(etat.vaisseauV.vitesseV <=0.) then etat.vaisseauV.vitesseV<- 0.
				   else etat.vaisseauV.vitesseV <-(vit-.dece))
                          else  etat.vaisseauV.vitesseV <-(vit-.dece) ; if(etat.vaisseauV.vitesseV <=0.) then etat.vaisseauV.vitesseV<- 0.;;

(*-----------------*)
(*--- rotations ---*)
(*-----------------*)

let rotationPoint etat (xpi,ypi) = let (x,y) = etat.vaisseauV.positionV in
		             let d = etat.vaisseauV.directionV in
			     let xp,yp = xpi,ypi in
			     ((cos d) *.(xp-.x)-.(sin d)*. (yp -. y)+.x),((sin (d)) *. (xp-.x) +. (cos (d)) *. (yp-.y) +. y) ;;

let rotationV etat = im.t<-(rotationPoint etat im.t); im.d<-(rotationPoint etat im.d); im.g<-(rotationPoint etat im.g); im.p1<-(rotationPoint etat im.p1); im.p2<-(rotationPoint etat im.p2); im.p3<-(rotationPoint etat im.p3); im.p4<-(rotationPoint etat im.p4); im.p5<-(rotationPoint etat im.p5);;

let rotation_gauche etat =etat.vaisseauV.directionV <- (etat.vaisseauV.directionV +.0.1745329251*.2.);etat;;

let rotation_droite etat = etat.vaisseauV.directionV <- (etat.vaisseauV.directionV -.0.1745329251*.2.);etat;;

let rotation3pisur4 etat (xpi,ypi) = let (x,y) = etat.vaisseauV.positionV in
		             let d = -.1.57079632679 in
			     let xp,yp = xpi,ypi in
			     ((cos d) *.(xp-.x)-.(sin d)*. (yp -. y)+.x),((sin (d)) *. (xp-.x) +. (cos (d)) *. (yp-.y) +. y) ;;

(*------------*)
(*--- tirs ---*)
(*------------*)

(*chaque tir i modifie la cadense de tir, le nombre de tir et sa taille*)
let tir0 etat = let xt,yt=im.t in
		let dir = etat.vaisseauV.directionV in
		(match etat.tir with

  |[]->if(etat.vaisseauV.chargementTir > 5) then (etat.vaisseauV.chargementTir <-0;(new_tir 0 (xt,yt) (dir-.1.57079632679) 30. )::etat.tir) else etat.tir
  |t::q -> if (etat.vaisseauV.chargementTir > 5) then (etat.vaisseauV.chargementTir <-0;(new_tir 0 (xt,yt) (dir-.1.57079632679) 30. )::etat.tir)
    else etat.tir) ;;

let tir1 etat =   let xt,yt=im.t in
		let dir = etat.vaisseauV.directionV in
		(match etat.tir with

  |[]->if(etat.vaisseauV.chargementTir > 10) then (etat.vaisseauV.chargementTir <-0; (new_tir 0 (xt,yt) (dir-.1.57079632679) 30. )::(new_tir 0 (xt,yt) dir 30. )::(new_tir 0 (xt,yt) (dir+.3.141) 30. )::etat.tir) else etat.tir
  |t::q -> if ( etat.vaisseauV.chargementTir > 10) then (etat.vaisseauV.chargementTir <-0;(new_tir 0 (xt,yt) (dir-.1.57079632679) 30. )::(new_tir 0 (xt,yt) dir 30. )::(new_tir 0 (xt,yt) (dir+.3.141) 30. )::etat.tir)
    else etat.tir) ;;

let tir2 etat =   let xt,yt=im.t in
		let dir = etat.vaisseauV.directionV in
		(match etat.tir with

  |[]->if(etat.vaisseauV.chargementTir > 1) then (etat.vaisseauV.chargementTir <-0; (new_tir 0 (xt,yt) (dir-.1.57079632679) 30. )::etat.tir) else etat.tir
  |t::q -> if (etat.vaisseauV.chargementTir > 1) then (etat.vaisseauV.chargementTir <-0;(new_tir 0 (xt,yt) (dir-.1.57079632679) 30. )::etat.tir)
    else etat.tir) ;;

let tir3 etat =   let xt,yt=im.t in
		let dir = etat.vaisseauV.directionV in
		(match etat.tir with
  |[]->if(etat.vaisseauV.chargementTir > 15) then (etat.vaisseauV.chargementTir <-0; (new_tir 0 (xt+.15.*.(cos dir),yt+.15.*.(sin dir)) (dir-.1.57079632679) 30. )::(new_tir 0 (xt+.30.*.(cos dir),yt+.30.*.(sin dir)) (dir-.1.57079632679) 30. )::(new_tir 0  (xt+.45.*.(cos dir),yt+.45.*.(sin dir)) (dir-.1.57079632679) 30. )::etat.tir) else etat.tir
  |t::q -> if (etat.vaisseauV.chargementTir > 10) then (etat.vaisseauV.chargementTir <-0;(new_tir 0 (xt+.15.*.(cos dir),yt+.15.*.(sin dir)) (dir-.1.57079632679) 30. )::(new_tir 0 (xt+.30.*.(cos dir),yt+.30.*.(sin dir)) (dir-.1.57079632679) 30. )::(new_tir 0  (xt+.45.*.(cos dir),yt+.45.*.(sin dir)) (dir-.1.57079632679) 30. )::etat.tir)
else etat.tir) ;;


let tir4 etat =   let xt,yt=im.t in
		let dir = etat.vaisseauV.directionV in
		(match etat.tir with

  |[]->if(etat.vaisseauV.chargementTir > 15) then (etat.vaisseauV.chargementTir <-0; ( new_tir 0 (xt,yt) (dir-.1.57079632679+.0.39269908169) 30.) ::(new_tir 0 (xt,yt)  (dir-.1.57079632679-.0.39269908169) 30. )::(new_tir 0 (xt,yt) (dir-.1.57079632679) 30.)::etat.tir) else etat.tir
  |t::q -> if (etat.vaisseauV.chargementTir > 5) then (etat.vaisseauV.chargementTir <-0;(new_tir 0 (xt,yt) (dir-.1.57079632679+.0.39269908169) 30. )::(new_tir 0 (xt,yt)  (dir-.1.57079632679-.0.39269908169) 30. )::(new_tir 0 (xt,yt) (dir-.1.57079632679) 30. )::etat.tir)
    else etat.tir) ;;

let tir5 etat =   let xt,yt=im.t in
		let dir = etat.vaisseauV.directionV in

(match etat.tir with

  |[]->if(etat.vaisseauV.chargementTir > 15) then (etat.vaisseauV.chargementTir <-0; new_tir 15 (xt,yt) (dir-.1.57079632679) 30. )::etat.tir else etat.tir
  |t::q -> if (etat.vaisseauV.chargementTir > 15) then (etat.vaisseauV.chargementTir <-0;(new_tir 15 (xt,yt) (dir-.1.57079632679) 30. )::etat.tir)
    else etat.tir) ;;

let tir6 etat =  let x,y = etat.vaisseauV.positionV in
                 let dir =etat.vaisseauV.directionV in

(match etat.tir with
  |[]->if(etat.vaisseauV.chargementTir > 40) then (etat.vaisseauV.chargementTir <-0; new_tir 25 (x,y) (dir-.1.57079632679) 0. )::etat.tir else etat.tir
  |t::q -> if (etat.vaisseauV.chargementTir > 40) then (etat.vaisseauV.chargementTir <-0;(new_tir 25 (x,y) (dir-.1.57079632679) 0. )::etat.tir)
    else etat.tir) ;;

(* tir d'un nouveau projectile *)
let tirer etat =match etat.jeu.inventaire.amelioration with 
  |0 -> tir0 etat
  |1 -> tir1 etat
  |2 -> tir2 etat
  |3 -> tir3 etat
  |4 -> tir4 etat
  |5 -> tir5 etat
  |6 -> tir6 etat
  |_ -> tir0 etat;;

let tir etat = etat.tir<-tirer etat;etat;;



let tailleAst ast = match ast.taille with
  |Petit -> 15.
  |Moyen -> 40.
  |Grand -> 80. ;;

(*-------------------*)  
(*--- Deplacement ---*)  
(*-------------------*)


(*deplacement ast : Lorsqu'il dépasse la zone de l'écran, il réapparait de l'autre coté*)
let avanceA astlist = List.map (fun ast ->let (a,b) = ast.positionA in 
					   	     let ax=a+.(cos ast.directionA *. ast.vitesseA) in
						     let bx= b+.(sin ast.directionA *.ast.vitesseA) in
						     let r = tailleAst ast in(
	if (ax>width+.r && bx>height+.r) then 
	   {ast with positionA=(0.-.(ax-.width),0.-.(bx-.height))} 
	else
	if (ax+.r<0. && bx+.r<0.) then 	 			
	   {ast with positionA=(width-.ax,height-.bx)} 
        else
	if (ax>width+.r && bx+.r<0.) then 
	   {ast with positionA=(0.-.(ax-.width),height-.bx)}                                                
        else
	if (ax+.r<0. && bx>height+.r)then 
	   {ast with positionA=(width+.ax,0.-.(bx-.height))}
        else
	if (ax+.r<0.) then						     
 	   {ast with positionA=(width-.ax,bx)}
        else 
	if (ax>width+.r)then 
           {ast with positionA=(0.-.(ax-.width),bx)}
	else 
	if (bx+.r<0.) then 						
	   {ast with positionA=(ax,height-.bx)}	  
        else
	if (bx>height+.r)then 				   
	   {ast with positionA=(ax,0.-.(bx-.height))} 
	else 
	   {ast with positionA=(ax,bx)} 
	))
                                      
                                      
 astlist ;;

let avanceAA etat = etat.ast <- avanceA etat.ast;;

(*deplacements tirs*)
let avanceT x=List.map (fun tir ->let (a,b) = tir.positionT in 
				  let ax=a+.(cos (tir.directionT+.1.57079632679) *. tir.vitesseT) and bx= b+.(sin (tir.directionT+.1.57079632679) *.tir.vitesseT) in
			{tir with persistance=(tir.persistance+1); positionT=(ax,bx)}
)
			       x ;;

let avanceTT etat = etat.tir <- avanceT etat.tir;;


(*met a jour chaque point du vaisseau*)
let miseajourDessinVaisseau etat = let a,b = etat.vaisseauV.positionV in 
                                   im.t <- a,20.+.b ;
                                   im.d <- a+.10.71,b-.20.;
                                   im.g <- a-.10.71,b-.20.;
				   im.p1 <- a-.5.,b;
				   im.p2 <- a-.2.,b-.20.;
				   im.p3 <- a,b-.10.;
				   im.p4 <- a+.2.,b-.30.;
				   im.p5 <- a+.5.,b;
				   im.t<- rotation3pisur4 etat im.t;
				   im.d<- rotation3pisur4 etat im.d;
				   im.g<- rotation3pisur4 etat im.g;
                                   im.p1 <- rotation3pisur4 etat im.p1;
                                   im.p2 <- rotation3pisur4 etat im.p2;
				   im.p3 <- rotation3pisur4 etat im.p3;
				   im.p4 <- rotation3pisur4 etat im.p4;
				   im.p5 <- rotation3pisur4 etat im.p5;;
                                   
			        
(*deplacement vaisseau, gère le cas où le vaisseau dépasse l'écran*)
let avanceVV v =  let a,b=v.positionV in
		  let ax=a+.(cos v.directionV *. v.vitesseV) 
		  and bx= b+.(sin v.directionV *.v.vitesseV) in
 (
	if (ax>width && bx>height) then 
	   {v with positionV=(ax-.width,bx-.height)} 
	else
	if (ax<0. && bx<0.) then 	 			
	   {v with positionV=(width-.ax,height-.bx)} 
        else
	if (ax>width && bx<0.) then 
	   {v with positionV=(ax-.width,height-.bx)}                                                
        else
	if (ax<0. && bx>height)then 
	   {v with positionV=(width-.ax,bx-.height)}
        else
	if (ax<0.) then						     
 	   {v with positionV=(width-.ax,bx)}
        else 
	if (ax>width)then 
           {v with positionV=(ax-.width,bx)}
	else 
	if (bx<0.) then 						
	   {v with positionV=(ax,height-.bx);}	  
        else
	if (bx>height)then 				   
	   {v with positionV=(ax,bx-.height)} 
	else 
	   {v with positionV=(ax,bx)}) ;;
	
let avanceV etat =etat.vaisseauV <- avanceVV etat.vaisseauV ;;

(*deplacement Bonus lors de la transition*)
let avanceBonus etat = List.map 
  (function bonus -> let (x,y) = bonus.positionB in {bonus with positionB = (x,(y+.2.))})
  etat.bonus ;;


(*--------------------*)
(* --- collisions --- *)
(*--------------------*)


(*delimite le cadre*)
let horsJeu (x,y) rayon = x >= width+.rayon || x <=(-.rayon) || y >=height+.rayon || y <=(-.rayon);;

(*suppression des objets (hors cadre pour le tir) ou detruit*)
let nettoyerTirlist tirlist = List.filter (fun tir ->( not tir.detruitT) && not (horsJeu tir.positionT 1.) ) tirlist ;;
let nettoyerAstlist astlist = List.filter (fun ast ->not ast.detruitA ) astlist ;;



(*determine une liste d'ast ayant ete touche par un tir*)
let astADetruir astlist =  List.filter (fun ast ->ast.detruitA ) astlist ;;



(*renvoie une position aléatoire dans le périmètre défini par l'ast passé en param*)
let positionAleatoire ast rayonAstCree rayonAstDetruit = let i = Random.int 16 in 
							 let x,y = ast.positionA in
						     match i with 
						       |0 -> x+.(rayonAstCree +. Random.float rayonAstDetruit),y+.(rayonAstCree +. Random.float rayonAstDetruit)
						       |1 -> x-.(rayonAstCree +. Random.float rayonAstDetruit),y+.(rayonAstCree +. Random.float rayonAstDetruit)
						       |2 -> x+.(rayonAstCree -. Random.float rayonAstDetruit),y+.(rayonAstCree +. Random.float rayonAstDetruit)
						       |3 -> x+.(rayonAstCree +. Random.float rayonAstDetruit),y-.(rayonAstCree +. Random.float rayonAstDetruit)
						       |4 -> x+.(rayonAstCree +. Random.float rayonAstDetruit),y+.(rayonAstCree -. Random.float rayonAstDetruit)
						       |5 -> x-.(rayonAstCree -. Random.float rayonAstDetruit),y+.(rayonAstCree +. Random.float rayonAstDetruit)
						       |6 -> x-.(rayonAstCree +. Random.float rayonAstDetruit),y-.(rayonAstCree +. Random.float rayonAstDetruit)
						       |7 -> x-.(rayonAstCree +. Random.float rayonAstDetruit),y+.(rayonAstCree -. Random.float rayonAstDetruit)
						       |8 -> x+.(rayonAstCree -. Random.float rayonAstDetruit),y-.(rayonAstCree +. Random.float rayonAstDetruit)
						       |9 -> x+.(rayonAstCree -. Random.float rayonAstDetruit),y+.(rayonAstCree -. Random.float rayonAstDetruit)
						       |10 -> x+.(rayonAstCree +. Random.float rayonAstDetruit),y-.(rayonAstCree -. Random.float rayonAstDetruit)
						       |11 -> x-.(rayonAstCree -. Random.float rayonAstDetruit),y-.(rayonAstCree +. Random.float rayonAstDetruit)
						       |12 -> x+.(rayonAstCree -. Random.float rayonAstDetruit),y-.(rayonAstCree -. Random.float rayonAstDetruit)
						       |13 -> x-.(rayonAstCree +. Random.float rayonAstDetruit),y-.(rayonAstCree -. Random.float rayonAstDetruit)
						       |14 -> x-.(rayonAstCree -. Random.float rayonAstDetruit),y+.(rayonAstCree -. Random.float rayonAstDetruit)
						       |15 -> x-.(rayonAstCree -. Random.float rayonAstDetruit),y-.(rayonAstCree -. Random.float rayonAstDetruit)
						       |_-> 0.,0.;;

let vitesseAleatoire ()=1. +. (Random.float 10.);;
						      

(*collision entre ast *)
let collisionAA ast1 ast2 = let acx,acy = ast1.positionA in
		       	let tcx,tcy = ast2.positionA in
			if( ((acx-.tcx) *. (acx-.tcx)) +. ((acy-.tcy) *. (acy -. tcy)) <= ((tailleAst ast1) +. (tailleAst ast2))*.((tailleAst ast1) +. (tailleAst ast2))) then true else false;;


(*genere list ast lors de la destruction dun gros et d'un Moyen -- elimine les positions qui implique une collision*)
let rec genAstDestru etat ast taille = function() ->

  let p1 = positionAleatoire ast taille (tailleAst ast) in
  let p2 = positionAleatoire ast taille (tailleAst ast) in
					     
  if(ast.taille = Moyen) then(
                                              
    let p3 = positionAleatoire ast taille (tailleAst ast) in
		
    let ast1 = (new_ast ast.couleur p1 (vitesseAleatoire ()) Petit (Random.float 6.28318530719 ) 0  ) in
					      
    let ast2 = (new_ast ast.couleur p2 (vitesseAleatoire ()) Petit (Random.float 6.28318530719 ) 0  ) in
					      
    let ast3 = (new_ast ast.couleur p3 (vitesseAleatoire ()) Petit (Random.float 6.28318530719 ) 0  ) in
					      
    if(collisionAA ast1 ast2) then genAstDestru etat ast taille ()

        else if ( collisionAA ast1 ast3) then genAstDestru etat ast taille()
					
	else if( collisionAA ast2 ast3) then genAstDestru etat ast taille ()
					
	else(ast1::ast2::ast3::[]))
					     
    else(
					
      let ast1 = (new_ast ast.couleur p1 (vitesseAleatoire ()) Moyen (Random.float 6.28318530719 ) 0 ) in
					    
      let ast2 = (new_ast ast.couleur p2  (vitesseAleatoire ()) Moyen (Random.float 6.28318530719 ) 0 ) in
					
      if (collisionAA ast1 ast2) then genAstDestru etat ast taille ()
					
      else (ast1::ast2::[])
    );;

(*Destruction d'un ast-> création de list d'ast plus petit ou rien si deja Petit*)
let rec destructionAst etat astlist =match astlist with
  |[]->[]
  |t::q ->(
    match t.taille with
      |Petit ->[]
      |Moyen -> genAstDestru etat t 15. () 
      |Grand -> genAstDestru etat t 40. ()
  )@destructionAst etat q;;
   

(*gestion collision 1 tir sur 1 ast*)
let collisionAT ast tir = let acx,acy = ast.positionA in
		       	let tcx,tcy = tir.positionT in
			if( ((acx-.tcx) *. (acx-.tcx)) +. ((acy-.tcy) *. (acy -. tcy)) <= ((tailleAst ast) +. (float_of_int tir.tailleT))*.((tailleAst ast) +. (float_of_int tir.tailleT))) then true else false;;
(*gestion collision vaisseau(assimilé à un cercle) sur ast*)
let collisionVA vai ast = let acx,acy = vai.positionV in
		       	let tcx,tcy = ast.positionA in
			if( ((acx-.tcx) *. (acx-.tcx)) +. ((acy-.tcy) *. (acy -. tcy)) <= ((tailleAst ast) +. 10.)*.((tailleAst ast) +. 10.)) then true else false;;


(*gestion collision d'un tir sur la list ast*)
let rec collisionta tir astlist = match astlist with
  |[]-> [tir]
  |t::q -> if(collisionAT t tir) then [{tir with detruitT=true}]
                                 else collisionta tir q;;

let rec collisionTa tirlist astlist = match tirlist with
  |[]->[]
  |t::q -> (collisionta t astlist)@(collisionTa q astlist);;


(*gere la collision d'un ast sur la liste de tir*)
 let rec collisionat ast tirlist = match tirlist with
  |[]-> [ast]
  |t::q -> if(collisionAT ast t)then [{ast with detruitA=true}]
                                 else collisionat ast q;;

let rec collisionAt astlist tirlist = match astlist with
  |[]->[]
  |t::q -> (collisionat t tirlist)@(collisionAt q tirlist);;



(*collision un ast sur astlist *)
let rec collisionAalist ast astlist = match astlist with
  |[]-> [ast]
  |t::q -> if(collisionAA t ast)&& t.colltmp =0 && ast.colltmp=0  then let dirt = t.directionA in 
								     let vit = t.vitesseA in 
[(new_ast t.couleur t.positionA ast.vitesseA t.taille ast.directionA 10);(new_ast ast.couleur ast.positionA vit ast.taille dirt 12)]
				 else collisionAalist ast q;;


(*gere collision inter ast en evitant de faire deux fois la meme collision  et en evitant de faire collision un ast avec un lui meme*)
let rec collisionalist astlist = match astlist with
  |[]->[]
  |r::q ->
    let l = collisionAalist r q in
	 (match l with
	   |[t]-> t::(collisionalist q)
	   |a::b::[]->    let xb,yb = b.positionA in
		       let xa,ya =a.positionA in
		       let listsansb = List.filter (function c -> let xc,yc = c.positionA in xc <> xb && yc <> yb) q in
		       let listsansab = List.filter (function c -> let xc,yc = c.positionA in xc <> xa && yc <> ya) listsansb in
	a::b::(collisionalist listsansab)
	   |_ -> []);;

(*gestion score*)
let rec cptscore etat astlist acc = match astlist with
  |[]-> acc
  |t::q -> (match t.taille with
      |Petit -> cptscore etat q (acc+20+(10*etat.jeu.niveau))
      |Moyen -> cptscore etat q (acc+10+(5*etat.jeu.niveau))
      |Grand -> cptscore etat q (acc+5+(5*etat.jeu.niveau))
  );;


(* gestion collision listast sur vaiseau *)
let rec collisionVa etat v astlist = match astlist with
  |[]-> v,(new_ast B (20000.,20000.) 0. Petit 0. 0)
  |t::q -> if (collisionVA v t ) && (etat.jeu.inventaire.bouclier) && etat.jeu.vulnerable = 40 then(
      etat.jeu.inventaire.bouclier <- false;
      etat.jeu.vulnerable <- 0;
      v,{t with detruitA = true })
           else if (collisionVA v t) && etat.jeu.vulnerable = 40 then (
	     etat.jeu.inventaire.vie <-(etat.jeu.inventaire.vie-1);
	     etat.jeu.vulnerable <-0;
	     etat.jeu.inventaire.amelioration <- 0;
	     etat.jeu.inventaire.dureeAm <- 0;
	     {v with positionV=cx,cy; directionV=0.;vitesseV=0.},{t with detruitA = true })
	   else collisionVa etat v q;;


(*collision bonus vaisseau*)
let collisionVB vai bon = let acx,acy = vai.positionV in
		       	let tcx,tcy = bon.positionB in
			if( ((acx-.tcx) *. (acx-.tcx)) +. ((acy-.tcy) *. (acy -. tcy)) <= (7. +. 10.)*.(7. +. 10.)) then true else false;;

(*gestion collision bonus vaisseau*)
let rec collisionVb etat v bonuslist = match bonuslist with
  |[]-> (new_bonus (20000.,20000.) 0 true)
  |t::q -> if (collisionVB v t) then (( match t.effet with
      |0-> etat.jeu.inventaire.vie <-(etat.jeu.inventaire.vie+1)
      |1-> etat.jeu.inventaire.dureeAm <- 400; etat.jeu.inventaire.amelioration <- ((Random.int 6)+1)
      |2-> etat.jeu.inventaire.bouclier <- true
      |_-> etat.jeu.inventaire.piece <- (etat.jeu.inventaire.piece+1));
	{t with detruitB = true })
    else collisionVb etat v q;;


(*collision boss vaisseau*)
let collisionBossVaisseau b v = let acx,acy = v.positionV in
		       	let tcx,tcy = b.positionBoss in
			if( ((acx-.tcx) *. (acx-.tcx)) +. ((acy-.tcy) *. (acy -. tcy)) <= (50. +. 10.)*.(50. +. 10.)) then true else false;;


(*gestion collision boss vaisseau *)
let collisionVBoss etat v bosslist = match bosslist with
  |[]-> v
  |t::q -> if (collisionBossVaisseau t v ) && (etat.jeu.inventaire.bouclier) && etat.jeu.vulnerable = 40 then(
      etat.jeu.inventaire.bouclier <- false;
      etat.jeu.vulnerable <- 0;
      v)
   else
      if (collisionBossVaisseau t v) && etat.jeu.vulnerable = 40 then(
		etat.jeu.inventaire.vie <-(etat.jeu.inventaire.vie-1);
		etat.jeu.vulnerable <-0;
		etat.jeu.inventaire.amelioration <- 0;
		etat.jeu.inventaire.dureeAm <- 0;
	{v with positionV=cx,cy; directionV=0.;vitesseV=0.})
        else v;;

(*collision boss tir *)
let collisionBossT tir b = let acx,acy = b.positionBoss in
		       	let tcx,tcy = tir.positionT in
			if( ((acx-.tcx) *. (acx-.tcx)) +. ((acy-.tcy) *. (acy -. tcy)) <= ((50. +. (float_of_int tir.tailleT))*.(50. +. (float_of_int tir.tailleT)))) then true else false;;

let rec collisionBossTirV etat tir bosslist =  match bosslist with
  |[]-> [tir]
  |t::q ->  if(collisionBossT tir t) then (t.vieBoss <- (t.vieBoss-1);t.teleport <- true; t.ancienpos <- t.positionBoss;  [{tir with detruitT=true}])
                                 else collisionBossTirV etat tir q;;
(*collision tirvaiseau boss*)
let rec collisionBTV etat tirlist bosslist = match tirlist with
  |[]->[]
  |t::q -> (collisionBossTirV etat t bosslist)@(collisionBTV etat q bosslist);;

(* gere collisions ast/tir tir/ast ast/ast*)
let collision etat  =  let astl1=collisionalist etat.ast in
		      let astl=collisionAt astl1 etat.tir in
		      let tirl1=collisionTa etat.tir etat.ast in
		      let tirl = collisionBTV etat tirl1 etat.boss in
		      let score = cptscore etat (astADetruir astl) 0 in
	       (((nettoyerAstlist astl)@(destructionAst etat (astADetruir astl))), (nettoyerTirlist tirl),score);;


(*supprime bonus recuperer par vaisseau *)
let rec nettoyerBonlist bon bonlist = match bonlist with
    |[]->[]
    |t::q -> let a,b= t.positionB in
	     let c,d = bon.positionB in
	     if (a=c && b=d) then q
	     else t::(nettoyerBonlist bon q);; 

(*collision entre un ast x et un ast y -> empeche collision de x et y avec un autre ast avant 0.5sec*)
let latenceCollAst astlist =List.map (fun ast -> if(ast.colltmp >0) then {ast with colltmp=ast.colltmp-1}else ast) astlist ;; 

(*passage niveau suivant *)
let niveauSuivant etat = 
  let niv = etat.jeu.niveau in  
  let astl = genAst niv () in
  let vie = 20*(int_of_float ((float_of_int niv)/.5.)) in
  let boss = new_boss (500.,500.) vie 20 0. 0. in
  if (niv mod 5 = 0) then
  (new_etat [] etat.vaisseauV [] [boss] etat.background etat.jeu)
  else new_etat [] etat.vaisseauV astl [] etat.background etat.jeu;;

(*gestion invulnerabilité vaisseau *)
let incrvul etat =
  if etat.jeu.vulnerable <> 40 then
    etat.jeu.vulnerable <- (etat.jeu.vulnerable +1)
  else ();;

(*supprime ast touche par le vaisseau*)
let rec destructionAstV ast astlist =
  match astlist with
    |[]->[]
    |t::q -> let a,b= t.positionA in
	     let c,d = ast.positionA in
	     if (a=c && b=d) then ast::q
	     else t::(destructionAstV ast q);; 

(*bonus apparait lors destruction d'un petit ast*)
let rec popBonus astlist =
  match astlist with
    |[]->[]
    |t::q -> (match t.taille with
	|Petit -> (genBonus t.positionA ())::(popBonus q)
	|_ -> popBonus q);;

(*limite duree d'un tir bonus *)
let dureeAmelioration etat =
  let d  = etat.jeu.inventaire.dureeAm in
  if d > 0 then (etat.jeu.inventaire.dureeAm <- (etat.jeu.inventaire.dureeAm -1);
		   if ((d-1) = 0) then etat.jeu.inventaire.amelioration <- 0);;

(*gestion de lattaque du boss *)
let tirBoss etat = match etat.boss with
  |[] ->()
  |t::q->           let x,y = t.positionBoss in
		   let xv,yv = etat.vaisseauV.positionV in
		   if(t.chargementTirBoss =0) then
		     (if (xv>= x) then(
		             let dir = atan((yv-.y)/.(xv-.x))+.(3.141) in
		             etat.ast <- (new_ast TirBoss (x,y) 10. Petit dir 0)::(new_ast TirBoss (x,y) 10. Petit (dir-.3.141) 0)::etat.ast;
			     t.chargementTirBoss <- 50;)
		      else ( let dir = atan((yv-.y)/.(xv-.x)) in
		     etat.ast <- (new_ast TirBoss (x,y) 10. Petit dir 0)::(new_ast TirBoss (x,y) 10. Petit (dir-.3.141) 0)::etat.ast;
		     t.chargementTirBoss <- 50;))
		   else t.chargementTirBoss <- (t.chargementTirBoss -1);;

(*supprime le boss si vie a 0*)
let detruitBoss bosslist = match bosslist with
  |[]->[]
  |t::q -> if (t.vieBoss =0)then [] else [t];;


(* génère des positions pour les étoiles à chaque 0.05s pour un effet de vitesse*)
let transitionNiveauSuivant etat = let back = genBackground 125 () in  etat.background.etoiles <- back;etat.jeu.inventaire.dureeAm <- etat.jeu.inventaire.dureeAm +1;;


let passageNewLvl etat = let x,y = etat.vaisseauV.positionV in
			 etat.background.newlvl <- (x<(2.*.width/.3.) && x>(width/.3.) && y >= height);;

(*--------------------*)
(*--- etat suivant ---*)
(*--------------------*)			 
			 
(*Regroupe le déplacement et les collisions pour état suivant*)			 
let etat_suivant etat =
if(not etat.jeu.gameover)then( 

if(etat.vaisseauV.vitesseV <=0.) then etat.vaisseauV.vitesseV<- 0.;
if(etat.jeu.pause ) then (etat) else (
if( not etat.background.newlvl ) then(
  deceleration etat;
  dureeAmelioration etat;
  avanceAA etat;
  avanceTT etat;
  avanceV etat;
  etat.vaisseauV.chargementTir <- (etat.vaisseauV.chargementTir +1);
  etat.ast <- latenceCollAst etat.ast;
  etat.vaisseauV <- collisionVBoss etat etat.vaisseauV etat.boss;
  let (v,tt) = collisionVa etat etat.vaisseauV etat.ast in
  etat.ast <- (destructionAstV tt etat.ast);
  etat.bonus <- etat.bonus@(popBonus (astADetruir(collisionAt etat.ast etat.tir)));
  etat.bonus <- nettoyerBonlist (collisionVb etat etat.vaisseauV etat.bonus) etat.bonus;
  let (a,t,s)=collision etat in
  etat.jeu.score <- (etat.jeu.score + s);
  incrvul etat;
  etat.vaisseauV <- v;
  etat.ast<-a;
  etat.tir<-t;
  tirBoss etat; etat.boss <- detruitBoss etat.boss;

if(etat.ast = [] && etat.boss = []) then (
    transitionNiveauSuivant etat; passageNewLvl etat;etat.bonus<-avanceBonus etat;etat) else etat
)
else (
      etat.background.newlvl <- false;
      etat.jeu.niveau <- (etat.jeu.niveau+1);
      etat.jeu.vulnerable <-20;
      etat.jeu.score <- (etat.jeu.score + 100*etat.jeu.niveau);
      niveauSuivant etat)
))
else etat;;

(*-------------------------------*)
(* --- affichages graphiques --- *)
(*-------------------------------*)


let couleurV etat = if (etat.jeu.vulnerable <>40)then (set_color (rgb 102 0 153);) 
                    else(set_color (rgb 104 131 139));;

(*dessine le vaisseau*)
let afficheV x =  miseajourDessinVaisseau x ; rotationV x;
                let xc,yc = x.vaisseauV.positionV in
                let xt,yt=im.t in
		let xd,yd=im.d in
		let xg,yg=im.g in
		let xp1f,yp1f = im.p1 in
		let xp2f,yp2f= im.p2 in
		let xp3f,yp3f= im.p3 in
		let xp4f,yp4f= im.p4 in
		let xp5f,yp5f= im.p5 in
		let xp1,yp1 = int_of_float xp1f,int_of_float yp1f in
		let xp2,yp2= int_of_float xp2f,int_of_float yp2f in
		let xp3,yp3= int_of_float xp3f,int_of_float yp3f in
		let xp4,yp4= int_of_float xp4f,int_of_float yp4f in
		let xp5,yp5= int_of_float xp5f,int_of_float yp5f in
		
		if(x.vaisseauV.vitesseV <> 0.) then( set_color red;
			   fill_poly[|xp1,yp1;xp2,yp2;xp3,yp3;xp4,yp4;xp5,yp5|]) 
                else();
		couleurV x;
		fill_poly [|((int_of_float xt),(int_of_float yt));((int_of_float xd),(int_of_float yd));((int_of_float xc),(int_of_float yc));((int_of_float xg),(int_of_float yg));((int_of_float xt),(int_of_float yt))|];;

(*match les possibilité de coleurs pour les asts*)		
let setcolor ast = match ast.couleur with
  |R->set_color(rgb 205 170 125)
  |B->set_color(rgb 205 102 29)
  |V->set_color(rgb 139 62 47)
  |J->set_color(rgb 139 69 0)
  |TirBoss->set_color cyan;;

(*dessine les astéroides*)
let rec afficheA astl =  match astl with
  |[]->()
  |t::q -> let a,b = t.positionA in 
				     (match t.taille with 
				       |Petit -> setcolor t;fill_circle (int_of_float a) (int_of_float b) 15;
afficheA q
				       |Moyen -> setcolor t;fill_circle (int_of_float a) (int_of_float b) 40;
afficheA q
				       |Grand -> setcolor t;fill_circle (int_of_float a) (int_of_float b) 80;
afficheA q ) ;;

(*dessine les tirs*)
let rec afficheT tirl = match tirl  with
  |[]->()
  |t::q -> let a,b = t.positionT in 
	   set_color white; draw_circle (int_of_float a) (int_of_float b) (int_of_float (float_of_int t.tailleT));
	   set_color (rgb 102 255 153) ;fill_circle (int_of_float a) (int_of_float b) (int_of_float (float_of_int t.tailleT));
	 set_color (rgb 51 255 102) ;fill_circle (int_of_float a) (int_of_float b) (int_of_float ((float_of_int t.tailleT)/.1.5));
 set_color (rgb 51 255 51) ;fill_circle (int_of_float a) (int_of_float b) (int_of_float ((float_of_int t.tailleT)/.2.5));
 set_color (rgb 0 255 0) ;fill_circle (int_of_float a) (int_of_float b) (int_of_float ((float_of_int t.tailleT)/.3.));afficheT q;;
  

(*dessine le fond étoilé*)
let afficheEtoile pos i= let a,b =pos in
		        if(i>=0 && i<25) then(
			  set_color (rgb 255 204 153);
			  fill_circle (int_of_float a) (int_of_float b) 2;
		          set_color white;
			  fill_circle (int_of_float a) (int_of_float b) 1)
			else ( if(i>=25 && i<50) then(
			       set_color (rgb 153 255 255) ;
			       fill_circle (int_of_float a) (int_of_float b) 2;
		          	set_color white;
			        fill_circle (int_of_float a) (int_of_float b) 1)
                                else( if(i>=50 && i<75) then (
			        set_color (rgb 255 80 80) ;
			        fill_circle (int_of_float a) (int_of_float b) 2;
		         	set_color white;
			        fill_circle (int_of_float a) (int_of_float b) 1)
			              else( if(i>=75 && i<100) then (
			                set_color yellow ;
					fill_circle (int_of_float a) (int_of_float b) 2;
		         	        set_color white;
					   fill_circle (int_of_float a) (int_of_float b) 1) 
					else(
			              set_color (rgb 255 255 153) ;
			              fill_circle (int_of_float a) (int_of_float b) 2;
			              set_color white;
			               fill_circle (int_of_float a) (int_of_float b) 1))));; 
	        

let rec afficheEtoiles listetoile i = match listetoile with 
  |[]-> afficheEtoile (0.,0.) i
  |t::q -> afficheEtoile t i; afficheEtoiles q (i+1);;

let afficheE listetoile i = afficheEtoiles listetoile i;;

(*affiche le score*)
let affiche_score etat = moveto 900 550; set_color white; draw_string (string_of_int etat.jeu.score);; 

(*affiche le niveau*)
let affiche_niveau etat = moveto 450 580; set_color white; draw_string "Niveau ";
moveto 500 580; draw_string (string_of_int etat.jeu.niveau);;

(*affiche le nombre de pièce et une pièce*)
let affiche_piece etat = set_color yellow; fill_circle 50 50 10; set_color (rgb 205 173 0);fill_circle 50 50 6;
moveto 75 45; set_color white; draw_string ":"; moveto 85 45; draw_string (string_of_int etat.jeu.inventaire.piece);;

(*affiche le nombre de vie avec un vaisseau a cote*)
let affiche_vie etat = 
let x,y=50.,550. in
let xt,yt = x, 20.+.y in
let xd,yd = x +.10.71, y-.20. in
let xg,yg= x-.10.71,y-.20. in
set_color (rgb 104 131 139);
		fill_poly [|((int_of_float xt),(int_of_float yt));((int_of_float xd),(int_of_float yd));((int_of_float x),(int_of_float y));((int_of_float xg),(int_of_float yg));((int_of_float xt),(int_of_float yt))|];
moveto 75 540; set_color white; draw_string ":"; moveto 85 540; draw_string (string_of_int etat.jeu.inventaire.vie);;


(*affiche le portail qui bouge pendant la transition de niveau*)
(*lpg,lpd varient et font bouger les ellipses*)
let affichePortail etat =set_color white;		 
fill_ellipse (int_of_float (width/.2.)) (-5) 177 (20-(int_of_float ((float_of_int etat.lpd)/.4.))+ (int_of_float ((float_of_int etat.lpg)/.4.))) ;
set_color (rgb 0 255 255); 
fill_ellipse (int_of_float (width/.2.)) (-5) 177 (16-(int_of_float ((float_of_int etat.lpd)/.4.))+ (int_of_float ((float_of_int etat.lpg)/.4.))) ;
set_color (rgb 0 204 255); 
fill_ellipse (int_of_float (width/.2.)) (-5) 177 (13-(int_of_float ((float_of_int etat.lpd)/.4.))+ (int_of_float ((float_of_int etat.lpg)/.4.))) ;
set_color (rgb 0 153 255); 
fill_ellipse (int_of_float (width/.2.)) (-5) 177 (9-(int_of_float ((float_of_int etat.lpd)/.4.))+ (int_of_float ((float_of_int etat.lpg)/.4.))) ;
set_color (rgb 0 102 255); 
fill_ellipse (int_of_float (width/.2.)) (-5) 177 (7-(int_of_float ((float_of_int etat.lpd)/.4.))+ (int_of_float ((float_of_int etat.lpg)/.4.))) ;
set_color (rgb 0 51 255); 
fill_ellipse (int_of_float (width/.2.)) (-5) 177 (5-(int_of_float ((float_of_int etat.lpd)/.4.))+ (int_of_float ((float_of_int etat.lpg)/.4.))) ;;


(*affiche une flèche pour guider pendant la transition de niveau*)
let afficheFleche etat = set_color (rgb 255 255 204);
fill_poly [|(500, 80);(480,100);(520,100);(500,80)|];
fill_rect 490 100 20 50;;

(*regarde si le highscore a été battu*)
let calculhighscore etat = if (etat.jeu.score > etat.jeu.highscore) then etat.jeu.highscore<- etat.jeu.score;;

(*Au game over, fait défilé le score jusqu'a arriver au score obteu*)
let affiche_score_final etat = if(etat.jeu.scorefinal < etat.jeu.score && etat.jeu.score >10) then etat.jeu.scorefinal <- (etat.jeu.scorefinal + (int_of_float (((float_of_int etat.jeu.score)*.5.)/.100.)))
  else (etat.jeu.scorefinal <- etat.jeu.score;calculhighscore etat);;

(*affiche l'écran de game over*)
let gameOver etat =etat.jeu.gameover <- true ; 
etat.ast <- []; etat.background.etoiles <- [];
affiche_score_final etat;
moveto 425 325; set_color white; set_text_size 10; draw_string "Game Over" ;
moveto 425 300; draw_string "Score : "; moveto 475 300; draw_string (string_of_int etat.jeu.scorefinal);
moveto 425 275; draw_string "Niveau final : "; moveto 518 275; draw_string (string_of_int etat.jeu.niveau);
moveto 425 250; draw_string "Highscore : "; moveto 500 250; draw_string (string_of_int etat.jeu.highscore);

moveto 425 225; draw_string "Press r to replay !";;

(*affiche la phrase Pause : p*)
let affiche_pause etat = moveto 25 500; draw_string "Pause : p ";;
    

(*affiche les bonus*)
let rec afficheBonus bonusl = match bonusl with
  |[]->()
  |t::q-> let a,b = t.positionB in 
	  (match t.effet with
          |0 -> set_color red; fill_circle (int_of_float a) (int_of_float b) 7; set_color(rgb 165 42 42);fill_circle (int_of_float a) (int_of_float b) 4 
	  |1 -> set_color blue; fill_circle (int_of_float a) (int_of_float b) 7; set_color(rgb 138 43 226);fill_circle (int_of_float a) (int_of_float b) 4
	  |2 -> set_color (rgb 51 102 0); fill_circle (int_of_float a) (int_of_float b) 7; set_color(rgb 51 0 51);fill_circle (int_of_float a) (int_of_float b) 4
	  |_ -> set_color yellow; fill_circle (int_of_float a) (int_of_float b) 7; set_color(rgb 205 173 0);fill_circle (int_of_float a) (int_of_float b) 4); afficheBonus q;;

(*affiche une phrase si on a + de 25 pièces*)
let affiche_piecea25 etat = if (etat.jeu.inventaire.piece >= 25) then (set_color white; moveto 150 45; draw_string "Appuyez sur v pour obtenir une vie !")else ();;

(*affiche le bouclier si possédé*)
let affiche_bouclier etat = if (etat.jeu.inventaire.bouclier) then let x,y = etat.vaisseauV.positionV in (set_color (rgb 51 102 0); draw_circle (int_of_float x) (int_of_float y) 21;)else ();;

(*affiche la barre de durée de l'amélioration*)
let affiche_timerAm etat = if (etat.jeu.inventaire.dureeAm > 200) then (set_color green) 
  else if (etat.jeu.inventaire.dureeAm <= 200 && etat.jeu.inventaire.dureeAm > 100) then (set_color (rgb 102 102 0))
  else if (etat.jeu.inventaire.dureeAm <= 100 && etat.jeu.inventaire.dureeAm > 50) then (set_color (rgb 255 102 0))
else(set_color red);
fill_rect 750 25 (int_of_float((float_of_int etat.jeu.inventaire.dureeAm)/.2.)) 25;
set_color (rgb 220 220 220);
draw_rect 750 25 200 25;;

(*-----------------*)
(*affichage du boss*)
(*-----------------*)

(*affiche les vies du boss au dessus de lui*)
let affiche_vieBoss etat = match etat.boss with 
  |[] -> ()
  |t::q -> let bx,by=t.positionBoss in (if ( (float_of_int (t.vieBoss)) > t.vieBossMax/.2.) then (set_color green) 
  else if ( (float_of_int t.vieBoss)<=t.vieBossMax/.2. && (float_of_int t.vieBoss)>t.vieBossMax/.4.) then (set_color (rgb 102 102 0))
  else if ((float_of_int t.vieBoss)>=t.vieBossMax/.4. && (float_of_int t.vieBoss)>t.vieBossMax/.8.) then (set_color (rgb 255 102 0))
else(set_color red);
fill_rect (int_of_float (bx-.25.)) (int_of_float (by+.53.)) (int_of_float ((float_of_int t.vieBoss)*.50./.t.vieBossMax )) 10;
set_color (rgb 220 220 220);
draw_rect (int_of_float (bx-.25.)) (int_of_float (by+.53.)) 50 10);;

(*dessine le boss, son bouclier et l'oeil qui regarde en direction du vaisseau*)
let affiche_bossaux etat i= match etat.boss with
  |[]->()
  |t::q ->              let x,y = t.positionBoss in
			let xv,yv = etat.vaisseauV.positionV in
			let rayon = 50. /. i in
		        if (t.vieBoss > (int_of_float (t.vieBossMax/.2.))) then(
			  set_color (rgb 220 220 220);
                        

		          fill_poly [|(int_of_float x),(int_of_float (y+.rayon));
				      (int_of_float (x+.rayon*.cos(3.141/.4.))),(int_of_float (y+.rayon*.sin(3.141/.4.)));
                                      (int_of_float (x+.rayon)),(int_of_float y);
                                      (int_of_float (x+.rayon*.cos(-.3.141/.4.))),(int_of_float (y+.rayon*.sin(-.3.141/.4.)));
				      (int_of_float x),(int_of_float (y-.rayon));
                                      (int_of_float (x+.rayon*.cos(-.3.*.3.141/.4.))),(int_of_float (y+.rayon*.sin(-.3.*.3.141/.4.)));
                                      (int_of_float (x-.rayon)),(int_of_float y);
                                      (int_of_float (x+.rayon*.cos(3.*.3.141/.4.))),(int_of_float (y+.rayon*.sin(3.*.3.141/.4.)));
                                      (int_of_float x),(int_of_float (y+.rayon))|];);

			  set_color (rgb 102 0 0);
			  fill_circle (int_of_float x) (int_of_float y) (int_of_float (rayon-.10./.i));
			  set_color red;
			  draw_circle (int_of_float x) (int_of_float y) (int_of_float (rayon-.10./.i));
			  set_color black;
			  let gr = int_of_float (20./.i) in
			  let pr = int_of_float (5./.i) in
			  if (xv>x -.50. && xv< x +.50. && y < yv) then
			  (fill_ellipse (int_of_float x) (int_of_float (y+.6.)) pr gr)
			  else if (xv>x -.50. && xv< x +.50. && y > yv) then
			  (fill_ellipse (int_of_float x) (int_of_float (y-.6.)) pr gr)
			  else if (x < xv && yv>y -.50. && yv< y +.50.) then
			  (fill_ellipse (int_of_float (x+.6.)) (int_of_float y) pr gr)
			  else if (x > xv && yv>y -.50. && yv< y +.50.) then
			  (fill_ellipse (int_of_float (x-.6.)) (int_of_float y) pr gr)
			  else if (x <xv && y < yv) then
			  (fill_ellipse (int_of_float (x+.6.)) (int_of_float (y+.6.)) pr gr)
			  else if (x<xv && y>= yv) then
			 ( fill_ellipse (int_of_float (x+.6.)) (int_of_float (y-.6.)) pr gr)
			  else if (x>=xv && y< yv) then
			 ( fill_ellipse (int_of_float (x-.6.)) (int_of_float (y+.6.)) pr gr)
			  else 
                         ( fill_ellipse (int_of_float (x-.6.)) (int_of_float (y-.6.)) pr gr);;

(*affiche l'animation de téléportation lorsque le boss est touché*)
let teleportation etat = match etat.boss with 
  |[] -> ()
  |t::q -> if(t.teleport) then (t.lattel <- t.lattel+.10.;
				if(t.lattel <= 100.) then(
           let lattel = t.lattel in
           let xa,ya = t.ancienpos in

	   set_color (rgb 0 255 255);
draw_circle (int_of_float xa) (int_of_float ya) (120-(int_of_float lattel));
draw_circle (int_of_float xa) (int_of_float ya) (165-(int_of_float (lattel*.1.5)));
draw_circle (int_of_float xa) (int_of_float ya) (200-(int_of_float (lattel*.2.)));
set_color (rgb 0 204 255);
draw_circle (int_of_float xa) (int_of_float ya) (119-(int_of_float lattel));
draw_circle (int_of_float xa) (int_of_float ya) (164-(int_of_float (lattel*.1.5)));
draw_circle (int_of_float xa) (int_of_float ya) (219-(int_of_float (lattel*.2.)));
set_color (rgb 0 153 255);
draw_circle (int_of_float xa) (int_of_float ya) (118-(int_of_float lattel));
draw_circle (int_of_float xa) (int_of_float ya) (163-(int_of_float (lattel*.1.5)));
draw_circle (int_of_float xa) (int_of_float ya) (218-(int_of_float (lattel*.2.)));
set_color (rgb 0 102 255);
draw_circle (int_of_float xa) (int_of_float ya) (117-(int_of_float lattel));
draw_circle (int_of_float xa) (int_of_float ya) (162-(int_of_float (lattel*.1.5)));
draw_circle (int_of_float xa) (int_of_float ya) (217-(int_of_float (lattel*.2.)));
set_color (rgb 0 51 255);
draw_circle (int_of_float xa) (int_of_float ya) (116-(int_of_float lattel));
draw_circle (int_of_float xa) (int_of_float ya) (161-(int_of_float (lattel*.1.5)));
draw_circle (int_of_float xa) (int_of_float ya) (216-(int_of_float (lattel*.2.)));)
	 
				else ()) else ();;



let affiche_boss etat = match etat.boss with
  |[] -> ()
  |t::q -> if (t.teleport) then ( teleportation etat;if(t.lattel=100.) then (t.positionBoss <- genPosBoss etat () ; t.lattel <- 0.; t.teleport <- false ;affiche_bossaux etat 1.)else if(t.lattel >=40. && t.lattel <80.) then affiche_bossaux etat 1.5 else if ( t.lattel >= 80. && t.lattel <100.) then affiche_bossaux etat 3. else affiche_bossaux etat 1.
  )else affiche_bossaux etat 1.;;
  
  
(*-------------------------------------------------------------*)
(*affiche tous les éléments du jeu en fonction de l'état du jeu*)
(*-------------------------------------------------------------*)

let affiche_etat etat = clear_graph();
set_color black;
fill_rect 0 0 1000 600;
if(etat.jeu.pause ) then (moveto 500 300 ; set_color white; draw_string "Pause";) else(
if(etat.jeu.inventaire.vie < 0)then (gameOver etat) else(

if(etat.ast = []) then(
if(etat.boss =[] ) then (
if(etat.lpg >=0 && etat.lpg <20 && etat.cycle ) then etat.lpg<-(etat.lpg+1)else ( 
  if(etat.lpd >=0 && etat.lpd <20 && etat.cycle ) then etat.lpd<-etat.lpd+1 else ( 
    etat.cycle<- false;
    if(etat.lpg >0 && etat.lpg <=20) then etat.lpg <-(etat.lpg-1) else (
      if(etat.lpd >0 && etat.lpd <=20) then etat.lpd <- (etat.lpd-1) else (
	etat.cycle<-true))));affichePortail etat;afficheFleche ()) else affiche_vieBoss etat ) else ();
afficheE etat.background.etoiles 0;
affiche_pause etat;
if(etat.boss <> [] ) then( affiche_vieBoss etat; affiche_boss etat) else ();
afficheT etat.tir;
afficheA etat.ast;
afficheBonus etat.bonus;
afficheV etat;
affiche_bouclier etat;
affiche_score etat;
affiche_piece etat;
affiche_vie etat;
affiche_piecea25 etat;
affiche_timerAm etat;
affiche_niveau etat));;


(*------------------------------*)
(* --- boucle d'interaction --- *)
(*------------------------------*)

(*Gère les maj*)
let rec boucle_interaction ref_etat =
  let status = wait_next_event [Key_pressed] in (* on attend une frappe clavier *)
  let etat = !ref_etat in (* on recupere l'etat courant *)
  let nouvel_etat = (* on definit le nouvel etat... *)
    match status.key with (* ...en fonction de la touche frappee *)
    | '1' | 'j' | 'J' -> if (etat.jeu.pause ) then etat else rotation_gauche etat (* rotation vers la gauche *)
    | '2' | 'k' | 'K'-> if (etat.jeu.pause ) then etat else acceleration etat (* acceleration vers l'avant *)
    | '3' | 'l' | 'L'-> if (etat.jeu.pause ) then etat else rotation_droite etat (* rotation vers la droite *)
    | ' ' -> if (etat.jeu.pause ) then etat else tir etat (* tir d'un projectile *)
    | 'q' | 'Q'->let canal_sortie = open_out "highscore.txt" in (*gère la sauvegarde du highscore dans le highscore.txt*)
	    let t = ((string_of_int etat.jeu.highscore)) in
	    output_string canal_sortie t;
	    close_out canal_sortie;
	    print_endline "Bye bye!"; exit 0 (* on quitte le jeux *)
    | 'r' | 'R' -> if(etat.jeu.gameover) then init_etat etat.jeu.highscore else (etat) (*recommence le jeu*)
    | 'v' | 'V'-> if(etat.jeu.inventaire.piece >=25)then (  (*A 25 pièces ou plus, on obtient une vie si on appuie sur v*)
etat.jeu.inventaire.piece <-(etat.jeu.inventaire.piece - 25);
etat.jeu.inventaire.vie <- (etat.jeu.inventaire.vie +1); etat) 
            else etat
    | 'p' | 'P'-> if(etat.jeu.pause && not(etat.jeu.gameover)) then etat.jeu.pause<- false else if not(etat.jeu.gameover)then etat.jeu.pause <- true else etat.jeu.pause <- false; etat
    | _ -> etat in (* sinon, rien ne se passe *)
  ref_etat := nouvel_etat; (* on enregistre le nouvel etat *)
  boucle_interaction ref_etat;; (* on se remet en attente de frappe clavier *)

(* --- fonction principale --- *)
    
let main () =
  (*On regarde s'il existe un fichier highscore*)
    if ( not(Sys.file_exists "highscore.txt")) then(
	(*Si non on le crée*)
    let canal_sortie = open_out "highscore.txt" in
    output_string canal_sortie ((string_of_int 0));
    close_out canal_sortie;);
	(*Si oui on l'ouvre et on prend le highscore*)
    let canal_entree = open_in "highscore.txt" in
    let h = int_of_string ((input_line canal_entree)) in
    close_in canal_entree;
  (* initialisation du generateur aleatoire *)
  Random.self_init ();
  (* initialisation de la fenetre graphique et de l'affichage *)
  open_graph (" " ^ string_of_int (int_of_float width) ^ "x" ^ string_of_int (int_of_float height));
  auto_synchronize false;
  (* initialisation de l'etat du jeu *)
  let ref_etat = ref (init_etat h) in (*init_etat a besoin maintenant de l'argument highscore pour être utilisé*)
  (* programmation du refraichissement periodique de l'etat du jeu et de son affichage *)
  let _ = Unix.setitimer Unix.ITIMER_REAL
    { Unix.it_interval = 0.05; (* tous les 1/20eme de seconde... *)
      Unix.it_value = 0.05 } in
     Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun _ ->
      affiche_etat !ref_etat; (* ...afficher l'etat courant... *)
      synchronize ();
      ref_etat := etat_suivant !ref_etat)); (* ...puis calculer l'etat suivant *)
  boucle_interaction ref_etat;; (* lancer la boucle d'interaction avec le joueur *)

let _ = main ();; (* demarrer le jeu *)