%% @doc Chess Move Generator: Header file
%% @end
%% 
%% Author François Cardinaux, CH 1207 Genève
%% Copyright 2011 François Cardinaux

% Noticeable values in 0x88 representation: 
-define(ROW_SPAN,             16).
-define(MOVE_UP,              16).
-define(MOVE_UP_LEFT,         15).
-define(MOVE_UP_RIGHT,        17).
-define(MOVE_UP_2,            32).
-define(MOVE_DOWN,           -16).
-define(MOVE_DOWN_LEFT,      -17).
-define(MOVE_DOWN_RIGHT,     -15).
-define(MOVE_DOWN_2,         -32).
-define(BOTTOM_LEFT_CORNER,    0).
-define(BOTTOM_RIGHT_CORNER,   7).
-define(TOP_LEFT_CORNER,     112).
-define(TOP_RIGHT_CORNER,    119).
                 
-define(CASTLING_ALL,         15).
-define(CASTLING_WHITE_KING,   8).
-define(CASTLING_WHITE_QUEEN,  4).
-define(CASTLING_BLACK_KING,   2).
-define(CASTLING_BLACK_QUEEN,  1).

% Positions of the elements inside the ntc_chess_piece record
-define(PIECE_RECORD_COLOR,    2).
-define(PIECE_RECORD_TYPE,     3).
-define(PIECE_RECORD_SQUARE,   4).

-type chess_piece_color()   :: 'black' | 'white'.
-type chess_piece_type()    :: 'pawn' | 'knight' | 'bishop' | 'rook' | 'queen' | 'king'.
-type chess_piece_square()  :: ?BOTTOM_LEFT_CORNER..?TOP_RIGHT_CORNER. % In 0x88 representation
-type castling()            :: 'false' | 'queen' | 'king'.

-record(ntc_chess_piece, {
            color                               :: chess_piece_color(), 
            type                                :: chess_piece_type(), 
            square                  = false     :: 'false' | chess_piece_square()}). 
            
-record(ntc_chess_position, {
            pieces                              :: [#ntc_chess_piece{}], 
            turn                                :: 'false' | chess_piece_color(), 
            allowedCastling         = 0         :: 0..?CASTLING_ALL, 
            enPassantSquare         = false     :: 'false' | chess_piece_square(), 
            halfMoveClock           = 0         :: integer(), 
            moveNumber              = 0         :: integer()}).
            
-record(ntc_chess_move, {
            from                                :: #ntc_chess_piece{},  
            to                                  :: #ntc_chess_piece{},             % May be a different piece, in case of promotion
            newPosition                         :: #ntc_chess_position{}, 
            castling            = false         :: castling(), 
            taken               = false         :: 'false' | #ntc_chess_piece{}}). % Not necessarily the same square as 'to' (en passant)
            
% Source: Programming Erlang, p 424
-define(NYI(X), (begin
                    io:format("*** NYI ~p ~p ~p~n", [?MODULE, ?LINE, X])
                 end)).
                 

