%% @doc Chess Move Generator
%% @end
%%
%% To compile: erlc -DTEST chessfold.erl
%% 
%% @author François Cardinaux, CH 1207 Genève
%% @copyright 2011 François Cardinaux

-module(chessfold).
    
% API
-export([
    player_color/1, 
    opponent_color/1,
    pieces/1,
    is_king_attacked/1,
    string_to_position/1, 
    position_to_string/1, 
    position_to_string_without_counters/1, 
    all_possible_moves/1,
    all_possible_moves_from/2,
    position_after_move/1, 
    move_origin/1,
    move_target/1,
    piece_color/1,
    piece_type/1,
    piece_square/1]).

-ifdef(TEST).

-export([
    square_reference/2,
    square_to_string/1,
    allowed_castling_value_to_string/1]).

-endif.

-include("chessfold.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(WIN_THRESHOLD, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MACROS AND CONSTANTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Source: Programming Erlang, p 424
-define(NYI(X), (begin
                    io:format("*** NYI ~p ~p ~p~n", [?MODULE, ?LINE, X])
                 end)).
                 
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
-define(PIECE_RECORD_COLOR,  2).
-define(PIECE_RECORD_TYPE,   3).
-define(PIECE_RECORD_SQUARE, 4).

% Attack- and delta-array and constants (source: Jonatan Pettersson (mediocrechess@gmail.com))
-define(ATTACK_NONE,    0). % Deltas that no piece can move
-define(ATTACK_KQR,     1). % One square up down left and right
-define(ATTACK_QR,      2). % More than one square up down left and right
-define(ATTACK_KQBwP,   3). % One square diagonally up
-define(ATTACK_KQBbP,   4). % One square diagonally down
-define(ATTACK_QB,      5). % More than one square diagonally
-define(ATTACK_N,       6). % Knight moves

% Formula: attacked_square - attacking_square + 128 = pieces able to attack

-define(ATTACK_ARRAY,
            [0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,2,0,0,0, % 0-19
             0,0,0,5,0,0,5,0,0,0,0,0,2,0,0,0,0,0,5,0, % 20-39
             0,0,0,5,0,0,0,0,2,0,0,0,0,5,0,0,0,0,0,0, % 40-59
             5,0,0,0,2,0,0,0,5,0,0,0,0,0,0,0,0,5,0,0, % 60-79
             2,0,0,5,0,0,0,0,0,0,0,0,0,0,5,6,2,6,5,0, % 80-99
             0,0,0,0,0,0,0,0,0,0,6,4,1,4,6,0,0,0,0,0, % 100-119
             0,2,2,2,2,2,2,1,0,1,2,2,2,2,2,2,0,0,0,0, % 120-139
             0,0,6,3,1,3,6,0,0,0,0,0,0,0,0,0,0,0,5,6, % 140-159
             2,6,5,0,0,0,0,0,0,0,0,0,0,5,0,0,2,0,0,5, % 160-179
             0,0,0,0,0,0,0,0,5,0,0,0,2,0,0,0,5,0,0,0, % 180-199
             0,0,0,5,0,0,0,0,2,0,0,0,0,5,0,0,0,0,5,0, % 200-219
             0,0,0,0,2,0,0,0,0,0,5,0,0,5,0,0,0,0,0,0, % 220-239
             2,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0]).     % 240-256

% Same as attack array but gives the delta needed to get to the square

-define(DELTA_ARRAY, 
            [  0,   0,   0,   0,   0,   0,   0,   0,   0, -17,   0,   0,   0,   0,   0,   0, -16,   0,   0,   0,
               0,   0,   0, -15,   0,   0, -17,   0,   0,   0,   0,   0, -16,   0,   0,   0,   0,   0, -15,   0,
               0,   0,   0, -17,   0,   0,   0,   0, -16,   0,   0,   0,   0, -15,   0,   0,   0,   0,   0,   0,
             -17,   0,   0,   0, -16,   0,   0,   0, -15,   0,   0,   0,   0,   0,   0,   0,   0, -17,   0,   0,
             -16,   0,   0, -15,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -17, -33, -16, -31, -15,   0,
               0,   0,   0,   0,   0,   0,   0,   0,   0,   0, -18, -17, -16, -15, -14,   0,   0,   0,   0,   0,
               0,  -1,  -1,  -1,  -1,  -1,  -1,  -1,   0,   1,   1,   1,   1,   1,   1,   1,   0,   0,   0,   0,
               0,   0,  14,  15,  16,  17,  18,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  15,  31,
              16,  33,  17,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  15,   0,   0,  16,   0,   0,  17,
               0,   0,   0,   0,   0,   0,   0,   0,  15,   0,   0,   0,  16,   0,   0,   0,  17,   0,   0,   0,
               0,   0,   0,  15,   0,   0,   0,   0,  16,   0,   0,   0,   0,  17,   0,   0,   0,   0,  15,   0,
               0,   0,   0,   0,  16,   0,   0,   0,   0,   0,  17,   0,   0,  15,   0,   0,   0,   0,   0,   0,
              16,   0,   0,   0,   0,   0,   0,  17,   0,   0,   0,   0,   0,   0,   0,   0,   0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -----------------------------------------------------------------------------
%% @doc Get the identifier of the current player, i.e. the player that will play 
%% the move FROM the current position
%% @spec player_color(tuple()) -> atom()
player_color(#ntc_chess_position{turn = Turn}) ->
    Turn;

%% @spec player_color(list()) -> atom()
player_color(PositionString) ->
    Position = string_to_position(PositionString),
    player_color(Position).
    
%% -----------------------------------------------------------------------------
%% @doc Get the identifier of the opponent. 
%% In case there are more two colors in the game, the returned color must be the one
%% against which the current player can win. In other words, it is the previous player's
%% color, except that a color must be returned at initial position as well. 
%% @spec opponent_color(term()) -> atom()
opponent_color(PositionTupleOrString) ->
    case player_color(PositionTupleOrString) of
        white -> black;
        _     -> white
    end.
    
%% -----------------------------------------------------------------------------
%% @doc Get the list of pieces of the position
%% @spec pieces(term()) -> atom()
pieces(#ntc_chess_position{pieces = Pieces}) ->
    Pieces;
pieces(PositionString) ->
    pieces(string_to_position(PositionString)).
    
%% -----------------------------------------------------------------------------
%% @doc Is the king attacked in the position
%% @spec is_king_attacked(term()) -> atom()
is_king_attacked(#ntc_chess_position{pieces = Pieces, turn = PlayerColor} = Position) ->
    is_square_in_attack(
        Pieces, 
        opponent_color(Position), 
        king_square(Pieces, PlayerColor));
        
is_king_attacked(PositionString) ->
    Position = string_to_position(PositionString),
    is_king_attacked(Position).
    
    
%% -----------------------------------------------------------------------------
%% @doc Transform a string into a position Tuple
%% @spec string_to_position(string()) -> ntc_chess_position()
string_to_position(String) ->

    try
    
        [BoardString, TurnString, AllowedCastlingString, EnPassant|Remaining] = string:tokens(String, " "),
        
        {HalfMoveClock, MoveNumber}   = case Remaining of
                                            [HmcChar, MnChar] -> {list_to_integer(HmcChar), list_to_integer(MnChar)};
                                            [HmcChar]         -> {list_to_integer(HmcChar), 1};
                                            []                -> {0, 1}
                                        end,
        
        % Process each element one by one
        Pieces            = board_string_to_pieces(BoardString),
        AllowedCastling   = allowed_castling_string_to_value(AllowedCastlingString),
        EnPassantSquare   = en_passant_string_to_square(EnPassant), 
        Turn              = case TurnString of
                                "w" -> white;
                                _   -> black
                            end,
    
        % Return the position
        #ntc_chess_position{
            pieces                  = Pieces, 
            turn                    = Turn, 
            allowedCastling         = AllowedCastling, 
            enPassantSquare         = EnPassantSquare, 
            halfMoveClock           = HalfMoveClock, 
            moveNumber              = MoveNumber}
        
    catch
        error:Reason -> {failed_to_parse_string, {Reason, String}}
    end.
    
    board_string_to_pieces(BoardString) ->
        % Reverse, so that the table SquareTabId is immediately ordered by square ID
        RowStrings = lists:reverse(string:tokens(BoardString, "/")), 
        row_strings_to_pieces(RowStrings).
    
    row_strings_to_pieces(RowStrings) -> row_strings_to_pieces(RowStrings, [], 0).
    row_strings_to_pieces([],         Pieces,8) -> Pieces;
    row_strings_to_pieces([],         _,     RowId) when RowId <  8   -> throw({too_many_rows_defined, RowId});
    row_strings_to_pieces([],         _,     RowId) when RowId >  8   -> throw({not_enough_rows_defined, RowId});
    row_strings_to_pieces(RowStrings, Pieces,RowId) ->
        [RowString | Remaining] = RowStrings,
        row_strings_to_pieces(
                                    Remaining, 
                                    square_chars_to_pieces(RowString, Pieces, RowId), 
                                    RowId + 1).
    
    square_chars_to_pieces(RowString, Pieces, RowId) -> square_chars_to_pieces(RowString, Pieces, RowId * ?ROW_SPAN, RowId * ?ROW_SPAN + 7). 
    square_chars_to_pieces([],        _,      CurrentSquareId, LastSquareIdOfRow) when CurrentSquareId - LastSquareIdOfRow > 1 -> throw({too_many_squares_defined, CurrentSquareId - LastSquareIdOfRow});                     
    square_chars_to_pieces([],        _,      CurrentSquareId, LastSquareIdOfRow) when CurrentSquareId - LastSquareIdOfRow < 1 -> throw({not_enough_squares_defined, CurrentSquareId - LastSquareIdOfRow});
    square_chars_to_pieces([],        Pieces, _,               _) -> Pieces;
    square_chars_to_pieces(RowString, Pieces, CurrentSquareId, LastSquareIdOfRow) ->
        [SquareCharCode | Remaining]  = RowString,
        {SquareIncrement, NewPiece}   = case SquareCharCode of
                                            $1 -> {1, empty_square}; 
                                            $2 -> {2, empty_square}; 
                                            $3 -> {3, empty_square}; 
                                            $4 -> {4, empty_square}; 
                                            $5 -> {5, empty_square}; 
                                            $6 -> {6, empty_square}; 
                                            $7 -> {7, empty_square}; 
                                            $8 -> {8, empty_square}; 
                                            $r -> {1, charcode_to_piece(SquareCharCode)};
                                            $n -> {1, charcode_to_piece(SquareCharCode)}; 
                                            $b -> {1, charcode_to_piece(SquareCharCode)}; 
                                            $q -> {1, charcode_to_piece(SquareCharCode)};
                                            $k -> {1, charcode_to_piece(SquareCharCode)};
                                            $p -> {1, charcode_to_piece(SquareCharCode)};
                                            $R -> {1, charcode_to_piece(SquareCharCode)};
                                            $N -> {1, charcode_to_piece(SquareCharCode)}; 
                                            $B -> {1, charcode_to_piece(SquareCharCode)}; 
                                            $Q -> {1, charcode_to_piece(SquareCharCode)};
                                            $K -> {1, charcode_to_piece(SquareCharCode)};
                                            $P -> {1, charcode_to_piece(SquareCharCode)};
                                            _ -> io:format("~nUnexpected character: ~w~n", [SquareCharCode])
                                        end, 
        NewPieces = case  NewPiece of
                        empty_square    -> Pieces;
                        _               -> [NewPiece#ntc_chess_piece{square = CurrentSquareId}|Pieces]
                    end, 
        square_chars_to_pieces(Remaining, NewPieces, CurrentSquareId + SquareIncrement, LastSquareIdOfRow).

    charcode_to_piece(CharCode) ->
        case CharCode of
            $r -> #ntc_chess_piece{color = black, type = rook  };
            $n -> #ntc_chess_piece{color = black, type = knight};
            $b -> #ntc_chess_piece{color = black, type = bishop};
            $q -> #ntc_chess_piece{color = black, type = queen };
            $k -> #ntc_chess_piece{color = black, type = king  };
            $p -> #ntc_chess_piece{color = black, type = pawn  };
            $R -> #ntc_chess_piece{color = white, type = rook  };
            $N -> #ntc_chess_piece{color = white, type = knight};
            $B -> #ntc_chess_piece{color = white, type = bishop};
            $Q -> #ntc_chess_piece{color = white, type = queen };
            $K -> #ntc_chess_piece{color = white, type = king  };
            $P -> #ntc_chess_piece{color = white, type = pawn  };
            _ -> false
        end.
    
    en_passant_string_to_square("-") -> false;
    en_passant_string_to_square([ColCode, RowCode]) when $a =< ColCode, ColCode =< $h, $1 =< RowCode, RowCode =< $8 ->
        ColValue = ColCode - $a,
        RowValue = RowCode - $1,
        square_reference(RowValue, ColValue);
    en_passant_string_to_square(EnPassant) -> throw({invalid_en_passant_string, EnPassant}).

    allowed_castling_string_to_value(AllowedCastlingString) ->
        Fun = fun(X, Value) -> 
            if 
                X == $K -> Value + ?CASTLING_WHITE_KING;
                X == $Q -> Value + ?CASTLING_WHITE_QUEEN;
                X == $k -> Value + ?CASTLING_BLACK_KING;
                X == $q -> Value + ?CASTLING_BLACK_QUEEN;
                true    -> Value
            end
        end,
        lists:foldl(Fun, 0, AllowedCastlingString).
        
%% -----------------------------------------------------------------------------
%% @doc Transform a position Tuple into string. 
%% Returns the Forsyth-Edwards representation of the position.
%% @spec position_to_string(ntc_chess_position()) -> string()
position_to_string(#ntc_chess_position{
        halfMoveClock     = HalfMoveClock, 
        moveNumber        = MoveNumber} = Position) ->
    
    string:join([position_to_string_without_counters(Position), integer_to_list(HalfMoveClock), integer_to_list(MoveNumber)], " ").
    
%% -----------------------------------------------------------------------------
%% @doc Transform a position Tuple into string. 
%% The returned string doesn't have any counter information (move count and half-move clock).
%% @spec position_to_string_without_counters(ntc_chess_position()) -> string()
position_to_string_without_counters(
    #ntc_chess_position{
        pieces            = Pieces, 
        turn              = Turn, 
        allowedCastling   = AllowedCastling, 
        enPassantSquare   = EnPassantSquare}) ->
        
    BoardString           = pieces_to_board_string(Pieces), 
    TurnChar              = case Turn of
                                white -> "w";
                                _     -> "b"
                            end,
    AllowedCastlingString = allowed_castling_value_to_string(AllowedCastling),
    EnPassant             = en_passant_square_to_string(EnPassantSquare), 
    
    % Return the Forsyth-Edwards representation of the position
    string:join([BoardString, TurnChar, AllowedCastlingString, EnPassant], " ").
    
    pieces_to_board_string(Pieces) ->
        RowStrings = pieces_to_row_strings(Pieces), 
        string:join(RowStrings, "/").
    
    pieces_to_row_strings(Pieces) -> pieces_to_row_strings(Pieces, 0, []).
    pieces_to_row_strings(_,      8,     Accumulator) -> Accumulator;
    pieces_to_row_strings(Pieces, RowId, Accumulator) when RowId < 0  -> throw({invalid_parameters, {Pieces, RowId, Accumulator}});
    pieces_to_row_strings(Pieces, RowId, Accumulator) when RowId > 8  -> throw({invalid_parameters, {Pieces, RowId, Accumulator}});
    pieces_to_row_strings(Pieces, RowId, Accumulator) ->
        NewRow = chess_grid_to_row_chars(Pieces, RowId),
        pieces_to_row_strings(
                            Pieces, 
                            RowId + 1, 
                            [NewRow | Accumulator]).
    
    chess_grid_to_row_chars(Pieces, RowId) -> chess_grid_to_row_chars(Pieces, RowId, 7, "", 0).
    chess_grid_to_row_chars(_, _, -1, Accumulator, Counter) ->
        Result    = case Counter of
                        0 -> Accumulator;
                        _ -> [integer_to_list(Counter) | Accumulator]
                    end, 
        string:join(Result, "");
        
    % Don't forget the row is parsed backwards!
    chess_grid_to_row_chars(Pieces, RowId, ColId, Accumulator, Counter) ->
        
        SquareKey = square_reference(RowId, ColId),
        
        PieceChar = case get_piece_on_square(Pieces, SquareKey) of 
                        false -> false;
                        Piece -> piece_to_char(Piece)
                    end,
    
        {NewAccumulator, NewCounter}  = case {PieceChar, Counter} of
                                            {false, _} -> {Accumulator, Counter + 1};
                                            {_    , 0} -> {[PieceChar | Accumulator], 0};
                                            _          -> {[PieceChar, integer_to_list(Counter) | Accumulator], 0}
                                        end, 
        
        chess_grid_to_row_chars(Pieces, RowId, ColId - 1, NewAccumulator, NewCounter).
        
    piece_to_char(#ntc_chess_piece{color = Color, type = Type}) ->
        case {Color, Type} of
            {black, rook}    -> "r";
            {black, knight}  -> "n";
            {black, bishop}  -> "b";
            {black, queen}   -> "q";
            {black, king}    -> "k";
            {black, pawn}    -> "p";
            {white, rook}    -> "R";
            {white, knight}  -> "N";
            {white, bishop}  -> "B";
            {white, queen}   -> "Q";
            {white, king}    -> "K";
            {white, pawn}    -> "P";
            _ -> false
        end.

    en_passant_square_to_string(false) -> "-";
    en_passant_square_to_string(SquareNumber) ->
        square_to_string(SquareNumber).
        
%% -----------------------------------------------------------------------------
%% @doc Get all possible moves from a specific position
%% @spec all_possible_moves(tuple()) -> list()
all_possible_moves(Position) when is_record(Position, ntc_chess_position) ->
    PseudoLegalMoves = all_pseudo_legal_moves(Position),
    eliminate_illegal_moves(PseudoLegalMoves);
all_possible_moves(PositionString) ->
    all_possible_moves(string_to_position(PositionString)).
    
    
    all_pseudo_legal_moves(Position) ->
        PlayerPieces = pieces_of_color(Position#ntc_chess_position.pieces, Position#ntc_chess_position.turn),
    
        AccumulatePseudoLegalMovesOfPiece = fun(PlayerPiece, MoveList) -> 
                                                accumulate_pseudo_legal_moves_of_piece(Position, PlayerPiece, MoveList)
                                            end, 
        lists:foldl(AccumulatePseudoLegalMovesOfPiece, [], PlayerPieces).
        
%% -----------------------------------------------------------------------------
%% @doc Get all possible moves from a specific position and a specific start square
%% @spec all_possible_moves(tuple()) -> list()
all_possible_moves_from(Position, StartPiece) when is_record(StartPiece, ntc_chess_piece) ->
    PseudoLegalMoves = accumulate_pseudo_legal_moves_of_piece(Position, StartPiece, []),
    eliminate_illegal_moves(PseudoLegalMoves);
all_possible_moves_from(Position, StartSquare) ->
    StartPiece = get_piece_on_square(Position#ntc_chess_position.pieces, StartSquare),
    all_possible_moves_from(Position, StartPiece).
        
    eliminate_illegal_moves(Moves) ->
        eliminate_illegal_moves(Moves, []). 
        
    eliminate_illegal_moves([],    LegalMoves)    -> LegalMoves;
    eliminate_illegal_moves(Moves, LegalMovesAcc) ->
        [Move|RemainingMoves] = Moves,
        
        % Determine if there is an attack *after* the move
        Pieces              = (Move#ntc_chess_move.newPosition)#ntc_chess_position.pieces,
        PlayerColor         = (Move#ntc_chess_move.from)#ntc_chess_piece.color,
        OpponentColor       = (Move#ntc_chess_move.newPosition)#ntc_chess_position.turn,
        
        % The king of the player who has *just played*, i.e. not the same king as if we called is_king_attacked on the resulting position
        KingSquare = king_square(Pieces, PlayerColor),
        PlayerKingAttacked  = is_square_in_attack(
                                    Pieces, 
                                    OpponentColor, 
                                    KingSquare),
                                    
        % In case of castling, verify the start and median square as well
        StartSquare = (Move#ntc_chess_move.from)#ntc_chess_piece.square,
        Illegal = case {PlayerKingAttacked, Move#ntc_chess_move.castling} of
            {true,      _} -> true;
            {false, false} -> false;
            {false,  king} -> is_any_square_in_attack(Pieces, OpponentColor, [StartSquare, StartSquare + 1]);
            {false, queen} -> is_any_square_in_attack(Pieces, OpponentColor, [StartSquare, StartSquare - 1])
        end,
        
        case Illegal of
            false -> eliminate_illegal_moves(RemainingMoves, [Move|LegalMovesAcc]);
            _     -> eliminate_illegal_moves(RemainingMoves, LegalMovesAcc)
        end.
        
        is_any_square_in_attack(_Pieces, _AttackingPieceColor,      []) -> false;
        is_any_square_in_attack( Pieces,  AttackingPieceColor, Targets) -> 
            [Target|RemainingTargets] = Targets,
            case is_square_in_attack(Pieces, AttackingPieceColor, Target) of
                true -> true;
                _    -> is_any_square_in_attack(Pieces, AttackingPieceColor, RemainingTargets)
            end.
        
%% -----------------------------------------------------------------------------
%% @doc Get the position after a move
%% @spec position_after_move(tuple()) -> list()
position_after_move(#ntc_chess_move{newPosition = NewPosition}) ->
    NewPosition.
    
%% -----------------------------------------------------------------------------
%% @doc Get the position after a move
%% @spec move_origin(tuple()) -> list()
move_origin(#ntc_chess_move{from = Origin}) ->
    Origin.
    
%% -----------------------------------------------------------------------------
%% @doc Get the position after a move
%% @spec move_target(tuple()) -> list()
move_target(#ntc_chess_move{to = Target}) ->
    Target.
    
%% -----------------------------------------------------------------------------
%% @doc Get the color of the piece
%% @spec piece_color(tuple()) -> atom()
piece_color(#ntc_chess_piece{color = Color}) ->
    Color.
    
%% -----------------------------------------------------------------------------
%% @doc Get the type of the piece
%% @spec piece_type(tuple()) -> atom()
piece_type(#ntc_chess_piece{type = Type}) ->
    Type.

%% -----------------------------------------------------------------------------
%% @doc Get the square of the piece
%% @spec piece_square(tuple()) -> atom()
piece_square(#ntc_chess_piece{square = Square}) ->
    Square.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Functions that are public in test mode and private in normal mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -----------------------------------------------------------------------------
%% @doc Square reference, key of the square dictionary
square_reference(RowId, ColId) ->
    ?ROW_SPAN * RowId + ColId. 
    
square_to_string(#ntc_chess_piece{square = SquareNumber}) ->
    square_to_string(SquareNumber);
square_to_string(SquareNumber) when SquareNumber > ?TOP_RIGHT_CORNER    -> throw({invalid_square_number, SquareNumber}); 
square_to_string(SquareNumber) when SquareNumber < ?BOTTOM_LEFT_CORNER  -> throw({invalid_square_number, SquareNumber});
square_to_string(SquareNumber) ->
    RowValue = SquareNumber div ?ROW_SPAN, 
    ColValue = SquareNumber rem ?ROW_SPAN, 
    [ColValue + $a, RowValue + $1].

allowed_castling_value_to_string(AllowedCastling) ->
    WQChar = case AllowedCastling band ?CASTLING_WHITE_QUEEN of 0 -> ""; _ -> "Q" end,
    WKChar = case AllowedCastling band ?CASTLING_WHITE_KING  of 0 -> ""; _ -> "K" end,
    BQChar = case AllowedCastling band ?CASTLING_BLACK_QUEEN of 0 -> ""; _ -> "q" end,
    BKChar = case AllowedCastling band ?CASTLING_BLACK_KING  of 0 -> ""; _ -> "k" end,
    AllowedCastlingString = string:join([WKChar, WQChar, BKChar, BQChar], ""),
    case AllowedCastlingString of 
        "" -> "-";
        _  -> AllowedCastlingString
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions related to squares
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -----------------------------------------------------------------------------
%% @doc Get the piece on the square 
%% @spec get_piece_on_square(tuple() or list(), integer()) -> atom() | false
get_piece_on_square(#ntc_chess_position{pieces = Pieces}, SquareKey) ->
    get_piece_on_square(Pieces, SquareKey);
    
get_piece_on_square(Pieces, SquareKey) when is_list(Pieces) ->
    lists:keyfind(SquareKey, ?PIECE_RECORD_SQUARE, Pieces). 
    
%% -----------------------------------------------------------------------------
%% @doc Is there a piece on the square?
%% @spec square_has_piece(tuple() or list(), integer()) -> boolean()
square_has_piece(#ntc_chess_position{pieces = Pieces}, Square) ->
    square_has_piece(Pieces, Square);

square_has_piece(Pieces, Square) when is_list(Pieces) ->
    Piece = get_piece_on_square(Pieces, Square),
    case Piece of
        false -> false;
        _     -> true
    end.

%% -----------------------------------------------------------------------------
%% @doc Is the specified square attacked by the specified color
is_square_in_attack(Pieces, AttackingPieceColor, AttackedSquare) ->
    OpponentPieces    = pieces_of_color(Pieces, AttackingPieceColor),
    IsPieceAnAttack   = fun(Piece, IsAlreadyAnAttack) ->
                            case IsAlreadyAnAttack of
                                true -> true;
                                _ ->
                                    AttackingSquare       = Piece#ntc_chess_piece.square,
                                    AttackArrayKey        = try AttackedSquare - AttackingSquare + 129 of % Not 128 because ?ATTACK_ARRAY keys start with 1
                                                                Value -> Value
                                                            catch
                                                                Error:Reason -> ?NYI({AttackingSquare, AttackedSquare, Error, Reason})
                                                            end,
                                    PiecesAbleToAttack    = lists:nth(AttackArrayKey, ?ATTACK_ARRAY),
                                    AttackingPieceType    = Piece#ntc_chess_piece.type,
                                    IsPossibleAttack      = case {PiecesAbleToAttack, AttackingPieceColor, AttackingPieceType} of
                                                                {?ATTACK_NONE,      _, _}      -> false;
                                                                {?ATTACK_KQR,       _, king}   -> true; 
                                                                {?ATTACK_KQR,       _, queen}  -> true; 
                                                                {?ATTACK_KQR,       _, rook}   -> true; 
                                                                {?ATTACK_QR,        _, queen}  -> true;
                                                                {?ATTACK_QR,        _, rook}   -> true;
                                                                {?ATTACK_KQBwP,     _, king}   -> true;
                                                                {?ATTACK_KQBwP,     _, queen}  -> true;
                                                                {?ATTACK_KQBwP,     _, bishop} -> true;
                                                                {?ATTACK_KQBwP, white, pawn}   -> true;
                                                                {?ATTACK_KQBbP,     _, king}   -> true;
                                                                {?ATTACK_KQBbP,     _, queen}  -> true;
                                                                {?ATTACK_KQBbP,     _, bishop} -> true;
                                                                {?ATTACK_KQBbP, black, pawn}   -> true;
                                                                {?ATTACK_QB,        _, queen}  -> true;
                                                                {?ATTACK_QB,        _, bishop} -> true;
                                                                {?ATTACK_N,         _, knight} -> true;
                                                                _ -> false
                                                            end,
                                                            
                                     case {IsPossibleAttack, AttackingPieceType} of
                                        {false, _}      -> false;
                                        {true,  pawn}   -> true;
                                        {true,  knight} -> true;
                                        {true,  king}   -> true;
                                        _ ->
                                            Increment = lists:nth(AttackArrayKey, ?DELTA_ARRAY),
                                            case is_piece_on_the_way(Pieces, AttackingSquare, AttackedSquare, Increment) of
                                                true -> false;
                                                _    -> true
                                            end
                                     end
                            end
                        end,
    lists:foldl(IsPieceAnAttack, false, OpponentPieces).
        
    is_piece_on_the_way(_Pieces, Square1, Square2, _Increment) when Square1 =:= Square2 -> false;
    is_piece_on_the_way(_Pieces, Square1, Square2,  Increment) when Square1 + Increment =:= Square2 -> false;
    is_piece_on_the_way( Pieces, Square1, Square2,  Increment) ->
        NewSquare1 = Square1 + Increment,
        case square_has_piece(Pieces, NewSquare1) of
            true -> true;
            _    -> is_piece_on_the_way( Pieces, NewSquare1, Square2,  Increment)
        end.
        
    pieces_of_color(Pieces, Color) ->
        IsColor = fun(Piece) ->
                            (Piece#ntc_chess_piece.color =:= Color)
                        end,
        lists:filter(IsColor, Pieces).
        
%% -----------------------------------------------------------------------------
%% @doc Where is the king of the specified color
king_square(Pieces, KingColor) ->
    IsPlayerKing  = fun(Piece) ->
                        case {Piece#ntc_chess_piece.color, Piece#ntc_chess_piece.type} of
                            {KingColor, king} -> true;
                            _                 -> false
                        end
                    end,
                    
    case lists:filter(IsPlayerKing, Pieces) of
        [PlayerKing] -> PlayerKing#ntc_chess_piece.square;
        _            -> ?NYI(Pieces), 
                        false
    end.
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions to compute moves
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -----------------------------------------------------------------------------
% Functions used by all_possible_moves() 
accumulate_pseudo_legal_moves_of_piece(Position, #ntc_chess_piece{color = PieceColor, type = PieceType} = MovedPiece, MoveListAcc) ->
    
    Opponent = opponent_color(Position),

    case {PieceColor, PieceType} of
        {Opponent, _}      -> MoveListAcc;
        {       _, pawn}   -> accumulate_pseudo_legal_pawn_moves(  Position, MovedPiece, MoveListAcc);
        {       _, rook}   -> accumulate_pseudo_legal_rook_moves(  Position, MovedPiece, MoveListAcc);
        {       _, knight} -> accumulate_pseudo_legal_knight_moves(Position, MovedPiece, MoveListAcc);
        {       _, bishop} -> accumulate_pseudo_legal_bishop_moves(Position, MovedPiece, MoveListAcc);
        {       _, queen}  -> accumulate_pseudo_legal_queen_moves( Position, MovedPiece, MoveListAcc);
        {       _, king}   -> accumulate_pseudo_legal_king_moves(  Position, MovedPiece, MoveListAcc);
        _                  -> throw({invalid_piece_type, PieceType})
    end.
    
accumulate_pseudo_legal_pawn_moves(#ntc_chess_position{turn = Turn, enPassantSquare = EnPassantSquare} = Position, #ntc_chess_piece{square = Square} = MovedPiece, MoveListAcc) ->

    {RowId, ColId} = {Square div ?ROW_SPAN, Square rem ?ROW_SPAN}, 
    
    NextRow   = case Turn of 
                    white -> RowId + 1; 
                    _     -> RowId - 1 
                end,
    
    {Increment, OtherIncrement, IsPromotion}  = case {Turn, RowId} of
                                        {white, 7} -> {false,       false,          false}; % absurd in normal play
                                        {black, 0} -> {false,       false,          false}; % absurd in normal play
                                        {white, 1} -> {?MOVE_UP,    ?MOVE_UP_2,     false};
                                        {white, 6} -> {?MOVE_UP,    false,          true};
                                        {black, 6} -> {?MOVE_DOWN,  ?MOVE_DOWN_2,   false};
                                        {black, 1} -> {?MOVE_DOWN,  false,          true};
                                        {white, _} -> {?MOVE_UP,    false,          false};
                                        {black, _} -> {?MOVE_DOWN,  false,          false}
                                    end,

    % Forward moves, including initial two-row move, resulting in en-passant for the next player
    
    {MoveListWithForward1, Forward2IsBlocked} = case Increment of
                                                    false -> {MoveListAcc, true};
                                                    
                                                    _ ->
                                                        NewSquare1 = Square + Increment,
                                                        case square_has_piece(Position, NewSquare1) of
                                                            true -> {MoveListAcc, true};
                                                            _    -> {insert_pseudo_legal_move(
                                                                                Position, 
                                                                                MovedPiece, 
                                                                                MovedPiece#ntc_chess_piece{square = NewSquare1}, 
                                                                                false, 
                                                                                false, 
                                                                                false,
                                                                                IsPromotion, 
                                                                                MoveListAcc), false}
                                                        end
                                                end,
                            
    MoveListWithForward2  = case {Forward2IsBlocked, OtherIncrement} of
                                {true, _}     -> MoveListWithForward1;
                                {_   , false} -> MoveListWithForward1;
                                _ ->
                                    NewSquare2 = Square + OtherIncrement,
                                    case square_has_piece(Position, NewSquare2) of
                                        true -> MoveListWithForward1;
                                        _    -> insert_pseudo_legal_move(
                                                            Position, 
                                                            MovedPiece, 
                                                            MovedPiece#ntc_chess_piece{square = NewSquare2}, 
                                                            false, 
                                                            false, 
                                                            Square + Increment,
                                                            false, 
                                                            MoveListWithForward1)
                                    end
                            end,
    
    % Taking moves
    
    Opponent = opponent_color(Position), 
    
    MoveListWithLeftTaking = case ColId of
        0 -> MoveListWithForward2;
        _ ->
            % Try to take on the left
            LeftSquare = square_reference(NextRow, ColId - 1),
            LeftTakenPiece = get_piece_on_square(Position, LeftSquare),
            case {LeftTakenPiece, EnPassantSquare} of
                {    _, LeftSquare} -> 
                    % En passant
                    insert_pseudo_legal_move(
                                    Position, 
                                    MovedPiece, 
                                    MovedPiece#ntc_chess_piece{square = LeftSquare}, 
                                    #ntc_chess_piece{color = Opponent, type = pawn, square = square_reference(RowId, ColId - 1)}, % taken en passant
                                    false, 
                                    false, 
                                    false, 
                                    MoveListWithForward2);
                    
                {false,          _} -> MoveListWithForward2;
                
                _ when LeftTakenPiece#ntc_chess_piece.color =:= Turn  -> MoveListWithForward2;
                
                _ -> 
                    insert_pseudo_legal_move(
                                    Position, 
                                    MovedPiece, 
                                    MovedPiece#ntc_chess_piece{square = LeftSquare}, 
                                    LeftTakenPiece, 
                                    false, 
                                    false, 
                                    IsPromotion, 
                                    MoveListWithForward2)
            end
    end,
    
    case ColId of
        7 -> MoveListWithLeftTaking;
        _ ->
            % Try to take on the right
            RightSquare = square_reference(NextRow, ColId + 1),
            RightTakenPiece = get_piece_on_square(Position, RightSquare),
            case {RightTakenPiece, EnPassantSquare} of
                {    _, RightSquare} ->  
                    % En passant
                    insert_pseudo_legal_move(
                                    Position, 
                                    MovedPiece, 
                                    MovedPiece#ntc_chess_piece{square = RightSquare}, 
                                    #ntc_chess_piece{color = Opponent, type = pawn, square = square_reference(RowId, ColId + 1)}, % taken en passant
                                    false, 
                                    false, 
                                    false, 
                                    MoveListWithLeftTaking);
                    
                {false,           _} -> MoveListWithLeftTaking;
                
                _ when RightTakenPiece#ntc_chess_piece.color =:= Turn  -> MoveListWithLeftTaking;
                
                _ -> 
                    insert_pseudo_legal_move(
                                    Position, 
                                    MovedPiece, 
                                    MovedPiece#ntc_chess_piece{square = RightSquare}, 
                                    RightTakenPiece, 
                                    false, 
                                    false, 
                                    IsPromotion, 
                                    MoveListWithLeftTaking)
            end
    end.

accumulate_pseudo_legal_rook_moves(#ntc_chess_position{turn = Turn} = Position, MovedPiece, MoveListAcc) ->
    Square = MovedPiece#ntc_chess_piece.square,
    accumulate_moves(Position, MovedPiece, Square,  ?ROW_SPAN, Turn, true, 
    accumulate_moves(Position, MovedPiece, Square, -?ROW_SPAN, Turn, true, 
    accumulate_moves(Position, MovedPiece, Square,   1, Turn, true, 
    accumulate_moves(Position, MovedPiece, Square,  -1, Turn, true, MoveListAcc)))).

accumulate_pseudo_legal_knight_moves(#ntc_chess_position{turn = Turn} = Position, MovedPiece, MoveListAcc) ->
    Square = MovedPiece#ntc_chess_piece.square,
    accumulate_moves(Position, MovedPiece, Square,  14, Turn, false, % 16 - 2 (0x88 representation)
    accumulate_moves(Position, MovedPiece, Square, -14, Turn, false, 
    accumulate_moves(Position, MovedPiece, Square,  18, Turn, false, % 16 + 2 (0x88 representation)
    accumulate_moves(Position, MovedPiece, Square, -18, Turn, false, 
    accumulate_moves(Position, MovedPiece, Square,  31, Turn, false, % 32 - 1 (0x88 representation)
    accumulate_moves(Position, MovedPiece, Square, -31, Turn, false, 
    accumulate_moves(Position, MovedPiece, Square,  33, Turn, false, % 32 + 1 (0x88 representation)
    accumulate_moves(Position, MovedPiece, Square, -33, Turn, false, MoveListAcc)))))))).

accumulate_pseudo_legal_bishop_moves(#ntc_chess_position{turn = Turn} = Position, MovedPiece, MoveListAcc) ->
    Square = MovedPiece#ntc_chess_piece.square,
    accumulate_moves(Position, MovedPiece, Square, ?MOVE_UP_LEFT,       Turn, true, 
    accumulate_moves(Position, MovedPiece, Square, ?MOVE_DOWN_RIGHT,    Turn, true, 
    accumulate_moves(Position, MovedPiece, Square, ?MOVE_UP_RIGHT,      Turn, true, 
    accumulate_moves(Position, MovedPiece, Square, ?MOVE_DOWN_LEFT,     Turn, true, MoveListAcc)))).

accumulate_pseudo_legal_queen_moves(Position, MovedPiece, MoveListAcc) ->
    accumulate_pseudo_legal_rook_moves(Position, MovedPiece,  
    accumulate_pseudo_legal_bishop_moves(Position, MovedPiece, MoveListAcc)).
    
king_side_castling(#ntc_chess_position{turn = Turn} = Position, MovedPiece, AllowedCastling, MoveListAcc) ->
    TurnKing  = case Turn of white -> ?CASTLING_WHITE_KING;  _ -> ?CASTLING_BLACK_KING  end, 
    case TurnKing band AllowedCastling of
        0 -> MoveListAcc;
        _ -> 
            Square = MovedPiece#ntc_chess_piece.square,
            PieceOnColumnF = get_piece_on_square(Position, Square + 1),
            PieceOnColumnG = get_piece_on_square(Position, Square + 2),
            if
                false =/= PieceOnColumnF -> MoveListAcc; 
                false =/= PieceOnColumnG -> MoveListAcc; 
                true                     -> insert_pseudo_legal_move(Position, MovedPiece, MovedPiece#ntc_chess_piece{square = Square + 2}, false, king, false, false, MoveListAcc)
            end
     end.
     
queen_side_castling(#ntc_chess_position{turn = Turn} = Position, MovedPiece, AllowedCastling, MoveListAcc) ->
    TurnQueen = case Turn of white -> ?CASTLING_WHITE_QUEEN; _ -> ?CASTLING_BLACK_QUEEN end, 
     case TurnQueen band AllowedCastling of 
         0 -> MoveListAcc;
         _ -> 
            Square = MovedPiece#ntc_chess_piece.square,
            PieceOnColumnD = get_piece_on_square(Position, Square - 1),
            PieceOnColumnC = get_piece_on_square(Position, Square - 2),
            PieceOnColumnB = get_piece_on_square(Position, Square - 3),
            if
                false =/= PieceOnColumnD -> MoveListAcc; 
                false =/= PieceOnColumnC -> MoveListAcc; 
                false =/= PieceOnColumnB -> MoveListAcc; 
                true                     -> insert_pseudo_legal_move(Position, MovedPiece, MovedPiece#ntc_chess_piece{square = Square - 2}, false, queen, false, false, MoveListAcc)
            end
     end.

accumulate_pseudo_legal_king_moves(
    #ntc_chess_position{
        turn                    = Turn, 
        allowedCastling         = AllowedCastling} = Position, 
    MovedPiece, 
    MoveListAcc) ->
    Square = MovedPiece#ntc_chess_piece.square,
    NormalMoveList = 
        accumulate_moves(Position, MovedPiece, Square, ?MOVE_UP,            Turn, false, 
        accumulate_moves(Position, MovedPiece, Square, ?MOVE_DOWN,          Turn, false, 
        accumulate_moves(Position, MovedPiece, Square,   1,                 Turn, false, 
        accumulate_moves(Position, MovedPiece, Square,  -1,                 Turn, false, 
        accumulate_moves(Position, MovedPiece, Square, ?MOVE_UP_LEFT,       Turn, false, 
        accumulate_moves(Position, MovedPiece, Square, ?MOVE_DOWN_RIGHT,    Turn, false, 
        accumulate_moves(Position, MovedPiece, Square, ?MOVE_UP_RIGHT,      Turn, false, 
        accumulate_moves(Position, MovedPiece, Square, ?MOVE_DOWN_LEFT,     Turn, false, MoveListAcc)))))))), 
    
    king_side_castling( Position, MovedPiece, AllowedCastling, 
    queen_side_castling(Position, MovedPiece, AllowedCastling, NormalMoveList)).
    
accumulate_moves(_,        _,          _,             0,         _,    _,        _)         -> throw({invalid_increment, 0});
accumulate_moves(Position, MovedPiece, CurrentSquare, Increment, Turn, Continue, MoveListAcc) ->
    NewSquare = CurrentSquare + Increment,
    
    case is_border_reached(NewSquare) of
        true -> MoveListAcc;
        _    -> OccupyingPiece = get_piece_on_square(Position, NewSquare),
                case OccupyingPiece of
                    #ntc_chess_piece{color = Turn} -> MoveListAcc;
                    false     -> NewMoveList = insert_pseudo_legal_move(Position, MovedPiece, MovedPiece#ntc_chess_piece{square = NewSquare}, false, false, false, false, MoveListAcc),
                                 if 
                                    Continue -> accumulate_moves(Position, MovedPiece, NewSquare, Increment, Turn, Continue, NewMoveList);
                                    true     -> NewMoveList
                                 end;
                    _         -> insert_pseudo_legal_move(Position, MovedPiece, MovedPiece#ntc_chess_piece{square = NewSquare}, OccupyingPiece, false, false, false, MoveListAcc)
                end
    end. 
    
    is_border_reached(Square) ->
        case (Square band 16#88) of
            0 -> false;
            _ -> true
        end.
    
% A promotion
insert_pseudo_legal_move(Position, From, To, Taken, _Castling, _NewEnPassant, true, MoveListAcc) ->
    insert_pseudo_legal_move(Position, From, To#ntc_chess_piece{type = queen},  Taken, false, false, false, 
    insert_pseudo_legal_move(Position, From, To#ntc_chess_piece{type = rook},   Taken, false, false, false, 
    insert_pseudo_legal_move(Position, From, To#ntc_chess_piece{type = bishop}, Taken, false, false, false, 
    insert_pseudo_legal_move(Position, From, To#ntc_chess_piece{type = knight}, Taken, false, false, false, MoveListAcc))));

% Not a promotion
insert_pseudo_legal_move(Position, From, To, Taken, Castling, NewEnPassant, false, MoveListAcc) ->
    NewPosition = get_new_position(Position, From, To, Taken, Castling, NewEnPassant), 
    
    Move = #ntc_chess_move{
                from        = From, 
                to          = To, 
                newPosition = NewPosition, 
                castling    = Castling, 
                taken       = Taken},
                
    [Move|MoveListAcc].
    
%% -----------------------------------------------------------------------------
%% @doc Execute the specified move and return the new ntc_chess_position tuple
%% @spec get_new_position(tuple(), tuple(), tuple(), tuple() or false, atom() or boolean()) -> tuple()
get_new_position(   #ntc_chess_position{
                        pieces                  = Pieces, 
                        turn                    = Turn, 
                        allowedCastling         = AllowedCastling, 
                        halfMoveClock           = HalfMoveClock, 
                        moveNumber              = MoveNumber}, 
                    From, To, Taken, Castling, NewEnPassant) ->
                
    % Delete taken piece
    NewPieces1 = case Taken of
                    false -> Pieces;
                    _     -> lists:keydelete(Taken#ntc_chess_piece.square, ?PIECE_RECORD_SQUARE, Pieces)
                 end,
    
    % Move piece
    NewPieces2 = move_piece(NewPieces1, From, To),
    
    % In case of castling, move the rook as well
    NewPieces3 = case Castling of
                    false -> NewPieces2;
                    queen -> 
                        RookSquare  = From#ntc_chess_piece.square - 4,
                        RookFrom    = #ntc_chess_piece{color = Turn, type = rook, square = RookSquare},
                        RookTo      = #ntc_chess_piece{color = Turn, type = rook, square = RookSquare + 3},
                        move_piece(NewPieces2, RookFrom, RookTo);
                    king  -> 
                        RookSquare  = From#ntc_chess_piece.square + 3,
                        RookFrom    = #ntc_chess_piece{color = Turn, type = rook, square = RookSquare},
                        RookTo      = #ntc_chess_piece{color = Turn, type = rook, square = RookSquare - 2},
                        move_piece(NewPieces2, RookFrom, RookTo)
                 end,
                   
    % Calculate new castling information
    EliminatedCastlingOfPlayer    = case {From#ntc_chess_piece.type, From#ntc_chess_piece.color, From#ntc_chess_piece.square} of 
                                        {king, white,                    _} -> ?CASTLING_WHITE_QUEEN bor ?CASTLING_WHITE_KING;
                                        {king,     _,                    _} -> ?CASTLING_BLACK_QUEEN bor ?CASTLING_BLACK_KING;
                                        {rook,     _,  ?BOTTOM_LEFT_CORNER} -> ?CASTLING_WHITE_QUEEN;
                                        {rook,     _, ?BOTTOM_RIGHT_CORNER} -> ?CASTLING_WHITE_KING;
                                        {rook,     _,     ?TOP_LEFT_CORNER} -> ?CASTLING_BLACK_QUEEN; 
                                        {rook,     _,    ?TOP_RIGHT_CORNER} -> ?CASTLING_BLACK_KING;  
                                        _                                   -> 0
                                    end,
      
    Filter1 = ?CASTLING_ALL bxor EliminatedCastlingOfPlayer,
    
    % Calculate the opponent's new castling information (if a tower is taken)
    EliminatedCastlingOfOpponent  = case Taken of 
                                        false -> 0;
                                        #ntc_chess_piece{color = VictimColor, type = VictimType} ->
                                            case VictimType of
                                                rook -> VictimLeftSquare = case VictimColor of 
                                                            black -> 112; 
                                                            _     -> 0 
                                                        end,
                                                        case To#ntc_chess_piece.square - VictimLeftSquare of
                                                            0 -> case VictimColor of white -> ?CASTLING_WHITE_QUEEN; _ -> ?CASTLING_BLACK_QUEEN end;
                                                            7 -> case VictimColor of white -> ?CASTLING_WHITE_KING;  _ -> ?CASTLING_BLACK_KING  end;
                                                            _ -> 0
                                                        end;
                                                _    -> 0
                                            end
                                    end,
                                 
    Filter2 = ?CASTLING_ALL bxor EliminatedCastlingOfOpponent,
    
    NewAllowedCastling = AllowedCastling band Filter1 band Filter2,

    % Update turn and move number
    {NewTurn, NewMoveNumber}  = case Turn of
                                    white -> {black, MoveNumber};
                                    _     -> {white, MoveNumber + 1}
                                end,
    
    % Update half-move clock
    NewHalfMoveClock  = if
                            Taken =/= false                     -> 0;
                            From#ntc_chess_piece.type =:= pawn -> 0;
                            true                                -> HalfMoveClock + 1
                        end,
    
    % Return the new position
    #ntc_chess_position{
            pieces                  = NewPieces3, 
            turn                    = NewTurn, 
            allowedCastling         = NewAllowedCastling, 
            enPassantSquare         = NewEnPassant, 
            halfMoveClock           = NewHalfMoveClock, 
            moveNumber              = NewMoveNumber
        }.
    
    move_piece(Pieces, From, To) ->
        % Remove the moved piece from its origin square
        NewPieces1 = lists:keydelete(From#ntc_chess_piece.square, ?PIECE_RECORD_SQUARE, Pieces),
        % Add the moved piece on target square
        [To|NewPieces1].
        

