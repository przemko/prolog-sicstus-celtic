/* -*- Mode:Prolog -*- */

/*
 * SICSTUS CLPFD AND TCL/TK DEMONSTRATION PROGRAM
 * Purpose   : Celtic Knotwork
 * Author    : Przemyslaw Kobylanski <przemko@mac.com>
 *
 * Program generates celtic knots with additional constraints for symmetry and
 * connectivity. The knot is drawn on the canvas. Below canvas there is menu 
 * which consists of check buttons for constraints (connectivity and line or 
 * point symmetry) and control buttons (Search, Next, Abort and Quit).
 * 
 * During execution program changes its state between SETTING parameters (with
 * check buttons), SEARCHING for solution and FINISHED (quit). Transitions are
 * performed on user clicks:
 *
 *          +--- Quit ---> [[FINISHED]] <--- Quit ---+
 *          |                                        |
 *          |                                        |
 * -->> [SETTING] ---------- Search ----------> [SEARCHING] -----+
 *          ^                                    |  ^            |
 *          |                                    |  |            |
 *          +--------------- Abort --------------+  +--- Next ---+
 * 
 * Original tiles graphics by Cameron Browne. Images from www page
 *
 *                http://www.cameronius.com/games/celtic/
 *
 * were converted to XBM format with ImageMagick.
 *
 * Balloon help for Tk widgets by Daniel Roche <daniel.roche@bigfoot.com>.
 *
 * EXAMPLE SESSION
 *
 * $ sicstus
 * SICStus 4.1.1 (x86_64-darwin-10.2.0): Tue Dec 15 16:13:48 CET 2009
 * Licensed to Przemyslaw Kobylanski on Mac
 * | ?- [celtic].
 * | ?- main.
 *
 * CHANGELOG
 *
 *   2010-01-23 version 2.0 by przemko@mac.com
 *                Connectivity
 *
 *   2010-01-20 version 1.2 by przemko@mac.com
 *                Balloons help by daniel.roche@bigfoot.com
 *
 *   2010-01-19 version 1.1 by przemko@mac.com
 *                Abort button
 *
 *   2010-01-19 version 1.0 by przemko@mac.com
 *                Solve and Quit buttons
 *
 *   2010-01-19 version 0.3 by przemko@mac.com
 *                Menu for symmetry
 *
 *   2010-01-19 version 0.2 by przemko@mac.com
 *                Line and point symmetry
 *
 *   2010-01-18 version 0.1 by przemko@mac.com
 *                Draw first solution
 *
 * TODO
 *
 *   - elimination mulitply solutions without generate and test (esp. in case 
 *     CON=1, LS=1, PS=1)
 *
 */

:- module(celtic, [main/0]).

:- use_module(library(lists)).
:- use_module(library(codesio)).
:- use_module(library(clpfd)).
:- use_module(library(tcltk)).

:- dynamic interp/1.

main :-
        retractall(interp(_)),
        tk_new([name('Celtic Knotwork 2.0 by Przemyslaw Kobylanski <przemko@mac.com>')], Interp),
        assert(interp(Interp)),
        tcl_eval(Interp, 'source balloon.tcl', _),
        tcl_eval(Interp, 'frame .m', _),
        tcl_eval(Interp, 'checkbutton .chc -text "connected" -variable connected -state normal', _),
        tcl_eval(Interp, 'checkbutton .chl -text "line symmetry" -variable line -state normal', _),
        tcl_eval(Interp, 'checkbutton .chp -text "point symmetry" -variable point -state normal', _),
        tcl_eval(Interp, 'button .b1 -text "Search" -state normal\
                                -command { prolog celtic:solve($connected,$line,$point) }', _),
        tcl_eval(Interp, 'button .b2 -text "Next" -command {prolog_event next} -state disable', _),
        tcl_eval(Interp, 'button .b4 -text "Abort" -command {prolog_event abort} -state disable', _),
        tcl_eval(Interp, 'button .b3 -text "Quit" -command exit -state normal', _),
        tcl_eval(Interp, 'canvas .c -width 520 -height 520', _),
        tcl_eval(Interp, 'pack .c', _),
        tcl_eval(Interp, 'pack .chc .chl .chp .b1 .b2 .b4 .b3 -in .m -side left', _),
        tcl_eval(Interp, 'pack .m', _),
        tcl_eval(Interp, 'set_balloon .b1  "Search for solutions"', _),
        tcl_eval(Interp, 'set_balloon .b2  "Find next solution"', _),
        tcl_eval(Interp, 'set_balloon .b4  "Abort searching"', _),
        tcl_eval(Interp, 'set_balloon .b3  "Quit application"', _),
        tcl_eval(Interp, 'set_balloon .chc "Switch on/off connectivity"', _),
        tcl_eval(Interp, 'set_balloon .chl "Switch on/off line symmetry (diagonal)"', _),
        tcl_eval(Interp, 'set_balloon .chp "Switch on/off point symmetry (center)"', _),
        tcl_eval(Interp, 'set_balloon .c   "Canvas for knot drawing"', _),
        tk_main_loop.

solve(CON,LS, PS) :-
        interp(Interp),
        tcl_eval(Interp, '.chc config -state disable', _),
        tcl_eval(Interp, '.chl config -state disable', _),
        tcl_eval(Interp, '.chp config -state disable', _),
        tcl_eval(Interp, '.b1  config -state disable', _),
        tcl_eval(Interp, '.b2  config -state normal', _),
        tcl_eval(Interp, '.b4  config -state normal', _),
        model(CON,LS, PS, Board, Vars1, Vars2),
        labeling([min], Vars1), % find knotwork
        once(labeling([min], Vars2)), % test connectivity
        draw_board(Board),
        tk_next_event(Interp, Event),
        (Event = next -> fail; finito).
solve(_, _, _) :-
        finito.

finito :-
        interp(Interp),
        tcl_eval(Interp, '.b1  config -state normal', _),
        tcl_eval(Interp, '.b2  config -state disable', _),
        tcl_eval(Interp, '.b4  config -state disable', _),
        tcl_eval(Interp, '.chc config -state normal', _),
        tcl_eval(Interp, '.chl config -state normal', _),
        tcl_eval(Interp, '.chp config -state normal', _).

draw_board(Board) :-
        interp(Interp),
        tcl_eval(Interp, 'foreach id [.c  find all] {.c delete $id }', _), 
        ( for(I, 0, 4), param(Board, Interp) do
            Y is 100*I+10,
          ( for(J, 0, 4), param(Board, Interp, I, Y) do
             X is 100*J+10,
             K is 5*I+J,
             nth0(K, Board, [SH, ROT, _, _, _, _]),
             format_to_codes('.c create bitmap ~w ~w -bitmap @pic/sh~w~w.xbm -anchor nw', 
                [X, Y, SH, ROT], Codes),
             atom_codes(Atom, Codes),
             tcl_eval(Interp, Atom, _)
          )
        ).

model(CON, LS, PS, Board, Vars1, Vars2) :-
        member(CON, [0, 1]),
        member(LS, [0, 1]),
        member(PS, [0, 1]),
        Board = [
                   B11, B12, B13, B14, B15,
                   B21, B22, B23, B24, B25,
                   B31, B32, B33, B34, B35,
                   B41, B42, B43, B44, B45,
                   B51, B52, B53, B54, B55
                ],
        tile(B11), tile(B12), tile(B13), tile(B14), tile(B15),
        tile(B21), tile(B22), tile(B23), tile(B24), tile(B25),
        tile(B31), tile(B32), tile(B33), tile(B34), tile(B35),
        tile(B41), tile(B42), tile(B43), tile(B44), tile(B45),
        tile(B51), tile(B52), tile(B53), tile(B54), tile(B55),
        (CON = 1 -> connected(Board, Vars2);  Vars2 = []),
        (LS = 1  -> line_symmetry(Board);  true),
        (PS = 1  -> point_symmetry(Board); true),
        shapes(Board, Shapes),
        global_cardinality(Shapes, [1-N1, 2-N2, 3-N3, 4-N4, 5-N5]),
        N is 5-LS*(1-PS)-PS*(1-LS)-5*LS*PS,
        N1 #>= N, N2 #>= N, N3 #>= N, N4 #>= N, N5 #>= N,
        north([B11, B12, B13, B14, B15]),
        south([B51, B52, B53, B54, B55]),
        west([B11, B21, B31, B41, B51]),
        east([B15, B25, B35, B45, B55]),
        left_right([B11, B12, B13, B14, B15]),
        left_right([B21, B22, B23, B24, B25]),
        left_right([B31, B32, B33, B34, B35]),
        left_right([B41, B42, B43, B44, B45]),
        left_right([B51, B52, B53, B54, B55]),
        up_down([B11, B21, B31, B41, B51]),
        up_down([B12, B22, B32, B42, B52]),
        up_down([B13, B23, B33, B43, B53]),
        up_down([B14, B24, B34, B44, B54]),
        up_down([B15, B25, B35, B45, B55]),
        append2(Board, Vars1).

tile([SH, ROT, N, E, S, W]) :-
        SH in 1..5,
        N in {0, 1},
        E in {0, 1},
        S in {0, 1},
        W in {0, 1},
        ROT in 0..3,
        table([[SH, ROT, N, E, S, W]],
              [
                 [1, 0, 0, 0, 1, 0],
                 [2, 0, 1, 0, 1, 0],
                 [3, 0, 0, 0, 1, 1],
                 [4, 0, 0, 1, 1, 1],
                 [5, 0, 1, 1, 1, 1],
                 [1, 1, 0, 0, 0, 1],
                 [2, 1, 0, 1, 0, 1],
                 [3, 1, 1, 0, 0, 1],
                 [4, 1, 1, 0, 1, 1],
%                [5, 1, 1, 1, 1, 1],
                 [1, 2, 1, 0, 0, 0],
%                [2, 2, 1, 0, 1, 0],
                 [3, 2, 1, 1, 0, 0],
                 [4, 2, 1, 1, 0, 1],
%                [5, 2, 1, 1, 1, 1],
                 [1, 3, 0, 1, 0, 0],
%                [2, 3, 0, 1, 0, 1],
                 [3, 3, 0, 1, 1, 0],
                 [4, 3, 1, 1, 1, 0]
%                [5, 3, 1, 1, 1, 1]
              ]).

connected(Board, YZ) :-
        Board = [
                   B11, B12, B13, B14, B15,
                   B21, B22, B23, B24, B25,
                   B31, B32, B33, B34, B35,
                   B41, B42, B43, B44, B45,
                   B51, B52, B53, B54, B55
                ],
        Y = [
               Y11, Y12, Y13, Y14, Y15,
               Y21, Y22, Y23, Y24, Y25,
               Y31, Y32, Y33, Y34, Y35,
               Y41, Y42, Y43, Y44, Y45,
               Y51, Y52, Y53, Y54, Y55
            ],
        Z = [
               Z11, Z12, Z13, Z14, Z15,
               Z21, Z22, Z23, Z24, Z25,
               Z31, Z32, Z33, Z34, Z35,
               Z41, Z42, Z43, Z44, Z45,
               Z51, Z52, Z53, Z54, Z55
            ],
        domain(Y, 0, 1),
        domain(Z, 1, 25),
        append(Y, Z, YZ),
        sum(Y, #=, 1),
        Y11 #= 1, % mozna tak przyjac jesli graf ma byc spojny
        neigh1(Z11, Y11, B11, Z12, Z21),
        neigh2(Z15, Y15, B15, Z25, Z24),
        neigh3(Z55, Y55, B55, Z54, Z45),
        neigh4(Z51, Y51, B51, Z41, Z52),
        neigh5(Z12, Y12, B12, Z13, Z22, Z11),
        neigh5(Z13, Y13, B13, Z14, Z23, Z12),
        neigh5(Z14, Y14, B14, Z15, Z24, Z13),
        neigh6(Z25, Y25, B25, Z35, Z24, Z15),
        neigh6(Z35, Y35, B35, Z45, Z34, Z25),
        neigh6(Z45, Y45, B45, Z55, Z44, Z35),
        neigh7(Z52, Y52, B52, Z51, Z42, Z53),
        neigh7(Z53, Y53, B53, Z52, Z43, Z54),
        neigh7(Z54, Y54, B54, Z53, Z44, Z55),
        neigh8(Z21, Y21, B21, Z11, Z22, Z31),
        neigh8(Z31, Y31, B31, Z21, Z32, Z41),
        neigh8(Z41, Y41, B41, Z31, Z42, Z51),
        neigh9(Z22, Y22, B22, Z12, Z23, Z32, Z21),
        neigh9(Z23, Y23, B23, Z13, Z24, Z33, Z22),
        neigh9(Z24, Y24, B24, Z14, Z25, Z34, Z23),
        neigh9(Z32, Y32, B32, Z22, Z33, Z42, Z31),
        neigh9(Z33, Y33, B33, Z23, Z34, Z43, Z32),
        neigh9(Z34, Y34, B34, Z24, Z35, Z44, Z33),
        neigh9(Z42, Y42, B42, Z32, Z43, Z52, Z41),
        neigh9(Z43, Y43, B43, Z33, Z44, Z53, Z42),
        neigh9(Z44, Y44, B44, Z34, Z45, Z54, Z43).

neigh1(Z, Y, [_,_,_,C1,C2,_], Z1, Z2) :-
        (Z #= Y) #\/ ((Z1 #= Z-1) #/\ C1) #\/ ((Z2 #= Z-1) #/\ C2).

neigh2(Z, Y, [_,_,_,_,C1,C2], Z1, Z2) :-
        (Z #= Y) #\/ ((Z1 #= Z-1) #/\ C1) #\/ ((Z2 #= Z-1) #/\ C2).

neigh3(Z, Y, [_,_,C2,_,_,C1], Z1, Z2) :-
        (Z #= Y) #\/ ((Z1 #= Z-1) #/\ C1) #\/ ((Z2 #= Z-1) #/\ C2).

neigh4(Z, Y, [_,_,C1,C2,_,_], Z1, Z2) :-
        (Z #= Y) #\/ ((Z1 #= Z-1) #/\ C1) #\/ ((Z2 #= Z-1) #/\ C2).

neigh5(Z, Y, [_,_,_,C1,C2,C3], Z1, Z2, Z3) :-
        (Z #= Y) #\/ ((Z1 #= Z-1) #/\ C1) #\/ ((Z2 #= Z-1) #/\ C2) #\/ ((Z3 #= Z-1) #/\ C3).

neigh6(Z, Y, [_,_,C3,_,C1,C2], Z1, Z2, Z3) :-
        (Z #= Y) #\/ ((Z1 #= Z-1) #/\ C1) #\/ ((Z2 #= Z-1) #/\ C2) #\/ ((Z3 #= Z-1) #/\ C3).

neigh7(Z, Y, [_,_,C2,C3,_,C1], Z1, Z2, Z3) :-
        (Z #= Y) #\/ ((Z1 #= Z-1) #/\ C1) #\/ ((Z2 #= Z-1) #/\ C2) #\/ ((Z3 #= Z-1) #/\ C3).

neigh8(Z, Y, [_,_,C1,C2,C3,_], Z1, Z2, Z3) :-
        (Z #= Y) #\/ ((Z1 #= Z-1) #/\ C1) #\/ ((Z2 #= Z-1) #/\ C2) #\/ ((Z3 #= Z-1) #/\ C3).

neigh9(Z, Y, [_,_,C1,C2,C3,C4], Z1, Z2, Z3, Z4) :-
        (Z #= Y) #\/ ((Z1 #= Z-1) #/\ C1) #\/ ((Z2 #= Z-1) #/\ C2) #\/ ((Z3 #= Z-1) #/\ C3) #\/ ((Z4 #= Z-1) #/\ C4).


line_symmetry(Board) :-
        Board = [
                   B11, B12, B13, B14, B15,
                   B21, B22, B23, B24, B25,
                   B31, B32, B33, B34, B35,
                   B41, B42, B43, B44, B45,
                   B51, B52, B53, B54, B55
                ],
        line_symmetry(B11, B11),
        line_symmetry(B12, B21),
        line_symmetry(B13, B31),
        line_symmetry(B14, B41),
        line_symmetry(B15, B51),
        line_symmetry(B22, B22),
        line_symmetry(B23, B32),
        line_symmetry(B24, B42),
        line_symmetry(B25, B52),
        line_symmetry(B33, B33),
        line_symmetry(B34, B43),
        line_symmetry(B35, B53),
        line_symmetry(B44, B44),
        line_symmetry(B45, B54),
        line_symmetry(B55, B55).

line_symmetry([_, _, N, E, S, W], [_, _, W, S, E, N]).

point_symmetry(Board) :-
        Board = [
                   B11, B12, B13, B14, B15,
                   B21, B22, B23, B24, B25,
                   B31, B32, B33, B34, B35,
                   B41, B42, B43, B44, B45,
                   B51, B52, B53, B54, B55
                ],
        point_symmetry(B11, B55),
        point_symmetry(B12, B54),
        point_symmetry(B13, B53),
        point_symmetry(B14, B52),
        point_symmetry(B15, B51),
        point_symmetry(B21, B45),
        point_symmetry(B22, B44),
        point_symmetry(B23, B43),
        point_symmetry(B24, B42),
        point_symmetry(B25, B41),
        point_symmetry(B31, B35),
        point_symmetry(B32, B34),
        point_symmetry(B33, B33).
 
point_symmetry([_, _, N, E, S, W], [_, _, S, W, N, E]).

shapes([], []).
shapes([[SH, _, _, _, _, _] | L1], [SH | L2]) :-
        shapes(L1, L2).

append2([], []).
append2([L1 | L2], L3) :-
        append(L1, L4, L3),
        append2(L2, L4).

north([]).
north([[_, _, 0, _, _, _] | L]) :-
        north(L).

east([]).
east([[_, _, _, 0, _, _] | L]) :-
        east(L).

south([]).
south([[_, _, _, _, 0, _] | L]) :-
        south(L).

west([]).
west([[_, _, _, _, _, 0] | L]) :-
        west(L).

left_right([]).
left_right([_]).
left_right([A, B | L]) :-
        left_right(A, B),
        left_right([B | L]).

left_right([_,_, _,X,_,_], [_,_, _,_,_,X]).

up_down([]).
up_down([_]).
up_down([A, B | L]) :-
        up_down(A, B),
        up_down([B | L]).

up_down([_,_, _,_,X,_], [_,_, X,_,_,_]).

