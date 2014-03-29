(* Algebraic Synchronizer by Elod P Csirmaz 2000

   This script implements some of the algorithms described in
   N. Ramsey and E. Csirmaz. An Algebraic Approach to File Synchronization.
   In Proceedings of the Joint 8th European Software Engineering Conference
   and 9th ACM SIGSOFT Symposium on the Foundations of Software Engineering.
   ACM Press, 2001. 175-185.
   Please also see <http://www.epcsirmaz.com/?q=algsyn>.

   This version is intended to work with unsion
   (unison is Copyright 1999-2012, Benjamin C. Pierce,
   please see <http://www.cis.upenn.edu/~bcpierce/unison/>).
   For a standalone version, please see Reconciler-Standalone.ml.
*)
(*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
(* update detector not ready, reconcilator implemented *)
(* This version uses the original Path.path representation
   but lacks the proper functions (see below) *)

module Reconcile (
  Fingerprints: sig
    type file_fp  (* completely characterizes file contents and metadata *)
    type dir_fp   (* completely characterizes directory metadata *)
  end)
( Path : sig
    type path

    (* constructors *)
    val empty : path
    val childPath : path -> string -> path (* childPath /a b = /a/b *)

    (* observers at leaf *)
    val parent : path -> path           (* parent /a/b/c = /a/b
                                           empty path is its own parent *)
    val path2finalname : path -> string option  (* last name, if nonempty *)

    (* observers at root *)
    val pathDeconstruct : path -> (string * path) option
                   (* pathDeconstruct /a/b/c/d = Some (a, /b/c/d) ???  *)

    (* other observer *)
    val path2string : path -> string
  end
) = struct

(* command: type of command, arguments and identifier (fingerprint)
   of the contents after modification *)
(**************************************)
  type command
    = Remove of Path.path
    | Edit   of Path.path * ctype
    | Create of Path.path * ctype
  and ctype
    = File of Fingerprints.file_fp
    | Dir  of Fingerprints.dir_fp 

(**
  type result = { perform : command list; conflicting : command list }

  type archive = Path.path -> ctype option

  let reconcile : archive -> command list -> command list -> result * result =
    fun archive r1 r2 -> assert false

  let string_from_ctype c = match c with
    File (xxx) -> "file"
  | Dir  (yyy) -> "directory"

  let contents archive pi =
    match archive pi with
      None   -> "Archive has nothing at " ^ Path.path2string pi
    | Some c -> "Archive has " ^ string_from_ctype c ^ " at " ^ Path.path2string pi

**)


(**** COMMANDS ON PATHS I NEED ********************************************)
(* Not complete, please revise *)

(* root : Path.path -> Path.path *)
(* gives the first node of the path. ( root(a/b/c) = a )
   root(emptypath)=emptypath *)
(* Note: we use relations <, >, = on entries *)
let root = fun p -> Path.empty

(* tail : Path.path -> Path.path *)
(* gives tail of path. ( tail(a/b/c) = b/c )
   tail(emptypath)=emptypath,
   also empty if path contained only one entry *)
let tail = fun p -> Path.empty

(* isempty : Path.path -> bool *)
(* true if path=emptypath *)
let isempty = fun p -> (1=1)

(* pathtostring : Path.path -> string *)
(* gives readable form of a path *)
let pathtostring p = ("Path")

(*** end *******************************************************************)

(* commandtostring : command -> string *)
(***************************************)
(* gives readable form of a command *)
let commandtostring = fun comm ->
  (match (comm) with
    (Remove x)          -> ("Rm  "^(pathtostring x))
  | (Create (x,File y)) -> ("CrF "^(pathtostring x))
  | (Create (x,Dir y))  -> ("CrD "^(pathtostring x))
  | (Edit (x,File y))   -> ("EdF "^(pathtostring x))
  | (Edit (x,Dir y))    -> ("EdD "^(pathtostring x)))

(* compresult: result of comparsion of paths *)
(*********************************************)
type compresult
  = Descendant
  | Precedant
  | Disjoint
  | Equivalent

(* compare : Path.path Path.path -> compresult *)
(***********************************************)
(* Determines the location of two points in a tree 
   gives relation of 2nd path according to the first one *)
let rec compare = fun p1 p2 ->
  match (root(p1),root(p2)) with
      (rp1,rp2) when (isempty(rp1) && isempty(rp2)) -> Equivalent
    | (rp1,rp2) when                  isempty(rp2)  -> Precedant
    | (rp1,rp2) when  isempty(rp1)                  -> Descendant
    | (rp1,rp2) when rp1=rp2                   -> compare (tail p1) (tail p2) 
    | (rp1,rp2)                                -> Disjoint

(* Extensions to compare *)
let isedesc = fun result -> (result = Descendant || result = Equivalent)
let iseprec = fun result -> (result = Precedant || result = Equivalent)

(* compabc : Path.path Path.path -> compresult *)
(***********************************************)
(* compares paths alphabetically (return value cannot be Disjoint) 
   gives relation of 2nd path according to the first one *)
let rec compabc = fun p1 p2 ->
  (*** end-mark! ***)
  match (root(p1),root(p2)) with 
      (rp1,rp2) when (isempty(rp1) && isempty(rp2)) -> Equivalent
    | (rp1,rp2) when                  isempty(rp2)  -> Precedant
    | (rp1,rp2) when  isempty(rp1)                  -> Descendant
    | (rp1,rp2) when rp1=rp2                   -> compabc (tail p1) (tail p2) 
    | (rp1,rp2) when rp1>rp2                   -> Precedant
    | (rp1,rp2) when rp1<rp2                   -> Descendant
    | (rp1,rp2) (*control never reaches this*) -> Disjoint (*Error*)

(*** updatedetector -- not complete yet ***)
(*
type extendedpath
  = Invalidpath
  | Realpath of Path.path *)
(* findroots: we do not need it *)
(*** end ***)

(* sortmode: determines order of paths *)
(***************************************)
(* used by comparepaths *)
type sortmode
  = Childrentoparent
  | Parenttochildren

(* comparepaths : Path.path -> Path.path -> sortmode -> (-/0/+) *)
(****************************************************************)
(* Compares paths according to mode, alphabetically
   (note that ordering the paths with this relation has the same 
   effect as the ordering method described in the paper) *)
let rec comparepaths = 
    (fun a b mode -> match ((compabc a b), mode) with
                (Equivalent,_)                -> 0
              | (Descendant,Parenttochildren) -> -1
              | (Descendant,Childrentoparent) -> 1
              | (Precedant,Parenttochildren)  -> 1
              | (Precedant,Childrentoparent)  -> -1
              | (Disjoint,Parenttochildren)  -> 2 (* CNRTL *)
              | (Disjoint,Childrentoparent)  -> -2 (* CNRTL *) )

(* pathof : command -> Path.path *)
(*********************************)
(* Gives the path of a command *)
let pathof = fun com ->
                 (match (com) with
                    (Remove x)     -> x
                  | (Create (x,y)) -> x
                  | (Edit (x,y))   -> x) 
  
(* sortcommands : command list -> command list *)
(***********************************************)
(* Orders commands in a minimal sequence as described in paper *)
let rec sortcommands = fun listofcommands ->
  (* trying find the commands in the list *)
  let creates =
  (List.filter (fun a -> (match (a) with (Create (x,y)) -> true | _ -> false))
               listofcommands) in
  let removes = 
  (List.filter (fun a -> (match (a) with (Remove x) -> true | _ -> false))
               listofcommands) in
  let edittodirs = 
  (List.filter (fun a -> 
    (match (a) with 
      (Edit (x,ct)) -> (match (ct) with (Dir z) -> true
                                      |  _      -> false)  
     | _            -> false))
    listofcommands) in
  let edittofiles = 
  (List.filter (fun a -> 
    (match (a) with 
      (Edit (x,ct)) -> (match (ct) with (File z) -> true
                                      |  _       -> false)
     | _            -> false))
    listofcommands) in

  (* now, after sorting them, we return the ordered list *)
  (
  (List.sort (fun a b -> (comparepaths (pathof a) (pathof b) Parenttochildren))
             edittodirs)
  @
  (List.sort (fun a b -> (comparepaths (pathof a) (pathof b) Childrentoparent))
             removes)
  @
  (List.sort (fun a b -> (comparepaths (pathof a) (pathof b) Parenttochildren))
             creates)
  @
  (List.sort (fun a b -> (comparepaths (pathof a) (pathof b) Parenttochildren))
             edittofiles)             
  )

(* dontcommute : command -> command -> bool *)
(********************************************)
(* true if commands don't commute according to the laws *)
let dontcommute = fun c1 c2 ->
  let rel = (compare (pathof c1) (pathof c2)) in
  (not(
    (* Law 7-15 (iv-ix) *)
    (rel = Disjoint) ||
    
    (* Law 1,2 (i,ip) *)
    ((match (c1,c2) with
       (Edit (p,q),Edit (r,s)) -> true
     | (_,_) -> false)
    && ((rel = Precedant) || (rel = Disjoint))) ||
    
    (* Law 4Ae (iib) *)
    ((match (c1,c2) with
       (Create (p,q),Edit (r,Dir s)) -> true
     | (_,_) -> false)
    && (rel = Precedant)) ||
    
    (* Law 5A (iiia) *)
    ((match (c1,c2) with
       (Edit (p,Dir q),Remove r) -> true
     | (_,_) -> false)
    && (rel = Descendant)) ||
    
    (* Law 6A (iiiap) *)
    ((match (c1,c2) with
       (Remove p,Edit (r,Dir s)) -> true
     | (_,_) -> false)
    && (rel = Precedant))
  ))

(* conflict : int(a) -> int(b) -> command list(a) -> command list(b) -> bool *)
(*****************************************************************************)
(* true if commands commandlistA[ia] and commandlistB[ib] are noncommuting
   and not present in the other sequence *)
(* Note that an edit or create command is equivalent (=) to
   another iff their fingerprints are the same ! *)
let conflict = fun ia ib seqa seqb ->
  ((dontcommute (List.nth seqa ia) (List.nth seqb ib)) &&
   (not (List.mem (List.nth seqa ia) seqb)) &&
   (not (List.mem (List.nth seqb ib) seqa)))

(* mustprecede : command(1) -> command(2) -> bool *)
(**************************************************)
(* true if command2 must precede command1 (!) *)
let mustprecede = fun c1 c2 ->
  let rel = (compare (pathof c1) (pathof c2)) in
  (
  (* Non-Law 43 (xxxvii) *)
  (match (c1,c2) with
    (Remove p,Remove q) -> (rel = Descendant)
  | (_,_) -> false) ||
  
  (* Non-Law 42 (xxxvi) *)
  (match (c1,c2) with
    (Create (p,q),Create (r,s)) -> (rel = Precedant)
  | (_,_) -> false) ||
  
  (* Non-Law 6B (iiic) *)
  (match (c1,c2) with
    (Edit (p, File q),Remove r) -> (rel = Descendant)
  | (_,_) -> false) ||
  
  (* Not-True-ELaw 3A (iibp) -- may be commutable *)
  (match (c1,c2) with
    (Create (p,q),Edit (r,File s)) -> (rel = Precedant)
  | (_,_) -> false)
  )

(* conflict descriptor *)
(***********************)
(* stores when the conflict was found (primary/derived)
   and the number of command with which the command conflicts *)   
type conflicttype
  = None
  | Primary of int
  | Derived of int

(* conflicttypetostring : conflicttype -> string *)
(*************************************************)
(* makes readable string, for debugging purposes *)
let conflicttypetostring = fun ctype ->
  match (ctype) with
    None        -> "None"
  (* command conflicts with another one from the other sequence *)
  | (Primary x) -> "<->"^(string_of_int x)
  (* command must be preceded by a command which already conflicts *)
  | (Derived x) -> "<=="^(string_of_int x)

(* extconflict : command list(a) -> command list(b) -> 
       (int, conflicttype) Hashtbl.t * (int, conflicttype) Hashtbl.t *)
(*********************************************************************)
(* Determines conflicts in two minimal, ordered sequences 
   Creates hash tables for storing conflicts 
   Hash tables bind the number of the command in the list to a
   conflict-descriptor *)
let extconflict = fun cla clb ->
  (
  let confla = (Hashtbl.create 100) in
  let conflb = (Hashtbl.create 100) in
  (
  (* seeking for primary conflicts in sequence A,
     that is, commands conflicting with a command in sequence B *)
  for ia = 0 to ((List.length cla)-1) do
    for ib = 0 to ((List.length clb)-1) do
      if (conflict ia ib cla clb) 
      then begin (Hashtbl.add confla ia (Primary(ib))) end
    done
  done;
  (* seeking for primary conflicts in sequence B *)
  for ib = 0 to ((List.length clb)-1) do
    for ia = 0 to ((List.length cla)-1) do
      if (conflict ia ib cla clb)
      then begin (Hashtbl.add conflb ib (Primary(ia))) end
    done
  done;
  (* seeking for secondary conflicts in sequence A,
     that is, commands which must be preceded by conflicting commands *)
  for i1 = 0 to ((List.length cla)-1) do
    for i2 = (i1+1) to ((List.length cla)-1) do
      (* if command sequence[i1] must precede sequence[i2] ... *)
      if ((mustprecede (List.nth cla i2) (List.nth cla i1)) &&
      (* ... AND sequence[i1] already conflicts: *)
         (Hashtbl.mem confla i1))
      then begin (Hashtbl.add confla i2 (Derived(i1))) end
    done;
  done;
  (* seeking for secondary (derived) conflicts in sequence B *)
  for i1 = 0 to ((List.length clb)-1) do
    for i2 = (i1+1) to ((List.length clb)-1) do
      if ((mustprecede (List.nth clb i2) (List.nth clb i1)) &&
         (Hashtbl.mem conflb i1))
      then begin (Hashtbl.add conflb i2 (Derived(i1))) end
    done;
  done;
  (* returning hash tables *)
  (confla, conflb)
  )
  )

(* getresult : command list -> (int, commandtype) Hashtbl.t -> 
                        command list(a) * command list(c)     *)  
(**************************************************************)
(* Split the sequence of commands into two sequences with the help of
   the conflict-hash *)
let getresult = fun clist chash ->
  (
  (* first, create a hash mapping conflicting commands to something *)
  let conflcomm = (Hashtbl.create 100) in
  (
  for i = 0 to (List.length clist) do
   if (Hashtbl.mem chash i)
   then begin (Hashtbl.add conflcomm (List.nth clist i) 1) end 
  done;
  (* creating a list of conflicting commands *)
  let conflictinglist =
  (List.filter (fun a -> (Hashtbl.mem conflcomm a)) clist) in
  (* creating a list of non-conflicting commands *)
  let appliablelist =
  (List.filter (fun a -> (not(Hashtbl.mem conflcomm a))) clist) in
  (* returning lists *)
  (appliablelist, conflictinglist)
  )
  )

(* reconciliator : command list(a) -> command list(b) ->
   command list(aC) * command list(bC) * 
   command list(Aoa) * command list(Aob)                     *)
(*************************************************************)
(* Splits two non-ordered minimal sequences to appliable (aA and bA)
   and non-appliable (conflicting, aC and bC) subsequences:
   aC: conflicting commands from sequence A
   bC: conflicting commands from sequence B
   Aoa: appliable commands from sequence B, on replica A
   Aob: appliable commands from sequence A, on replica B
   *)
let reconciliator = fun lista listb ->
  (
  (* first, we order the sequences *)
  let elista = (sortcommands lista) in
  let elistb = (sortcommands listb) in
  (
  (* now print them out for debugging purposes *)
  print_string "--Sequence A--\n";
  for i = 0 to ((List.length elista)-1) do
    print_string ((string_of_int i)^" "^
      (commandtostring (List.nth elista i))^"\n");
  done;
  print_string "--Sequence B--\n";
  for i = 0 to ((List.length elistb)-1) do
    print_string ((string_of_int i)^" "^
      (commandtostring (List.nth elistb i))^"\n");
  done;

  (* now generate conflict hashes *)
  let chashes = (extconflict elista elistb) in
    (* decompose tuple *)
  let chasha = (match chashes with (ha,hb) -> ha) in
  let chashb = (match chashes with (ha,hb) -> hb) in
  (
  (* now print out conflicts *)
  print_string "--Conflicts in A--\n";
  for i = 0 to ((List.length elista)-1) do
    if (Hashtbl.mem chasha i) then begin
      print_string ((string_of_int i)^
        (conflicttypetostring (Hashtbl.find chasha i))^"\n");
    end
  done;
  print_string "--Conflicts in B--\n";
  for i = 0 to ((List.length elistb)-1) do
    if (Hashtbl.mem chashb i) then begin
      print_string ((string_of_int i)^
        (conflicttypetostring (Hashtbl.find chashb i))^"\n");
    end
  done;

  (* now creating command lists *)
  (* get nonconflicting commands *)
  let sequencesfroma = (getresult elista chasha) in
   (* appliable commands from sequence a *)
   let appliablea = (match sequencesfroma with (s1,s2) -> s1) in
   (* conflicting commands from sequence a *)
   let conflictsa = (match sequencesfroma with (s1,s2) -> s2) in
  let sequencesfromb = (getresult elistb chashb) in
   (* same for b *)
   let appliableb = (match sequencesfromb with (s1,s2) -> s1) in
   let conflictsb = (match sequencesfromb with (s1,s2) -> s2) in
  (* leave out already applied ones *)
  (* appliable command from sequence b on replica a *)
  let appliableona =
   (List.filter (fun a -> (not(List.mem a elista))) appliableb) in
  (* appliable commands from sequence a on replica b *)
  let appliableonb =
   (List.filter (fun a -> (not(List.mem a elistb))) appliablea) in
  (
  (* print out appliable commands *)
  print_string "--Appliable on A--\n";
  for i = 0 to ((List.length appliableona)-1) do
    print_string ((commandtostring (List.nth appliableona i))^"\n");
  done;
  print_string "--Appliable on B--\n";
  for i = 0 to ((List.length appliableonb)-1) do
    print_string ((commandtostring (List.nth appliableonb i))^"\n");
  done;
  print_string "--- End ---\n"; 
  
  (* returning the lists *)
  (conflictsa,conflictsb,appliableona,appliableonb)
  )
  )
  )
  )
    

end 