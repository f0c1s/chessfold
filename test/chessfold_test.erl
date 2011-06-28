%% @doc Tests of the Chess Move Generator
%% @end
%%
%% This module is released under the GNU General Public License (GPL) version 3.
%% 
%% @author François Cardinaux, CH 1207 Genève
%% @copyright 2011 François Cardinaux

-module(chessfold_test).
    
-import(chessfold, [
    string_to_position/1, 
    position_to_string/1, 
    position_to_string_without_counters/1, 
    opponent_color/1,
    player_color/1, 
    all_possible_moves/1,
    square_to_string/1,
    allowed_castling_value_to_string/1]).
    
-include_lib("eunit/include/eunit.hrl").

-define(PERFT_DEPTH, 2). % Depth 6 OK
-define(PERFT_SUITE, "../test_data/perftsuite.txt").
-define(DO_NON_EVOLUTIVE_TESTS, false).
-define(DO_ISOLATION_TESTS, false).

% r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 ;D1 48 ;D2 2039 ;D3 97862 ;D4 4085603 ;D5 193690690

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

allowed_castling_value_to_string_test_() ->
    [
    fun() ->
        ?assert(allowed_castling_value_to_string(15) =:= "KQkq"),
        ?assert(allowed_castling_value_to_string(14) =:= "KQk"),
        ?assert(allowed_castling_value_to_string(13) =:= "KQq"),
        ?assert(allowed_castling_value_to_string(12) =:= "KQ"),
        ?assert(allowed_castling_value_to_string(11) =:= "Kkq"),
        ?assert(allowed_castling_value_to_string(10) =:= "Kk"),
        ?assert(allowed_castling_value_to_string(9)  =:= "Kq"),
        ?assert(allowed_castling_value_to_string(8)  =:= "K"),
        ?assert(allowed_castling_value_to_string(7)  =:= "Qkq"),
        ?assert(allowed_castling_value_to_string(6)  =:= "Qk"),
        ?assert(allowed_castling_value_to_string(5)  =:= "Qq"),
        ?assert(allowed_castling_value_to_string(4)  =:= "Q"),
        ?assert(allowed_castling_value_to_string(3)  =:= "kq"),
        ?assert(allowed_castling_value_to_string(2)  =:= "k"),
        ?assert(allowed_castling_value_to_string(1)  =:= "q"),
        ?assert(allowed_castling_value_to_string(0)  =:= "-")
    end
    ].


%% -----------------------------------------------------------------------------
abc_test_() ->
    [
    fun() ->
        case ?DO_NON_EVOLUTIVE_TESTS of
            true ->
                TestData = [
                    {   % Specific position
                        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 0",
                        [
                        % King moves
                        "8/2p5/K2p4/1P5r/1R3p1k/8/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/1P5r/KR3p1k/8/4P1P1/8 b - - 1 0",

                        % Rook moves   
                        "8/2p5/3p4/KP5r/R4p1k/8/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/2R2p1k/8/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/3R1p1k/8/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/4Rp1k/8/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/5R1k/8/4P1P1/8 b - - 0 0",
                        "8/2p5/3p4/KP5r/5p1k/1R6/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/5p1k/8/1R2P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/5p1k/8/4P1P1/1R6 b - - 1 0",
                        
                        %Pawn moves
                        "8/2p5/3p4/KP5r/1R3p1k/4P3/6P1/8 b - - 0 0",
                        "8/2p5/3p4/KP5r/1R2Pp1k/8/6P1/8 b - e3 0 0",
                        "8/2p5/3p4/KP5r/1R3p1k/6P1/4P3/8 b - - 0 0",
                        "8/2p5/3p4/KP5r/1R3pPk/8/4P3/8 b - g3 0 0"
                        ]
                    }
                ],
                all_possible_next_positions_test_acc(TestData);
            _ -> do_nothing  % ?DO_NON_EVOLUTIVE_TESTS
        end
    end
    ].
        
%% -----------------------------------------------------------------------------
abc2_test_() ->
    [
    fun() ->
        case ?DO_NON_EVOLUTIVE_TESTS of
            true ->
                TestData = [
                    {   % Specific position
                        "Rr2k2r/8/8/8/8/8/8/4K2R b KQk - 1 1", 
                        [
                        "e8g8",
                        "e8f8",
                        "e8d8",
                        "e8e7",
                        "e8d7",
                        "e8f7",
                        "b8c8",
                        "b8d8",
                        "b8a8",
                        "h8g8",
                        "h8f8",
                        "h8h7",
                        "h8h6",
                        "h8h5",
                        "h8h4",
                        "h8h3",
                        "h8h2",
                        "h8h1"
                        ]
                    },
                    {   % Specific position
                        "8/3K4/2p5/p2b2r1/5k2/8/8/1q6 b - - 1 67", 
                        [
                            "f4g4",
                            "f4e4",
                            "f4f3",
                            "f4f5",
                            "f4e3",
                            "f4g3",
                            "f4e5",
                            "c6c5",
                            "a5a4",
                            "d5c4",
                            "d5b3",
                            "d5a2",
                            "d5e6",
                            "d5f7",
                            "d5g8",
                            "d5e4",
                            "d5f3",
                            "d5g2",
                            "d5h1",
                            "g5h5",
                            "g5f5",
                            "g5e5",
                            "g5g4",
                            "g5g3",
                            "g5g2",
                            "g5g1",
                            "g5g6",
                            "g5g7",
                            "g5g8",
                            "b1c1",
                            "b1d1",
                            "b1e1",
                            "b1f1",
                            "b1g1",
                            "b1h1",
                            "b1a1",
                            "b1c2",
                            "b1d3",
                            "b1e4",
                            "b1f5",
                            "b1g6",
                            "b1h7",
                            "b1b2",
                            "b1b3",
                            "b1b4",
                            "b1b5",
                            "b1b6",
                            "b1b7",
                            "b1b8",
                            "b1a2"
                        ]
                    },
                    {   % Specific position
                        "r3k2r/p1ppq1b1/bn2pnp1/4N2Q/1p2P3/2N4p/PPPBBPPP/R3K2R b KQkq - 1 2", 
                        [
                            "e8g8",
                            "e8c8",
                            "e8f8",
                            "e8d8",
                            "c7c6",
                            "c7c5",
                            "d7d6",
                            "d7d5",
                            "g6h5",
                            "b4b3",
                            "b4c3",
                            "h3g2",
                            "a8b8",
                            "a8c8",
                            "a8d8",
                            "h8g8",
                            "h8f8",
                            "h8h7",
                            "h8h6",
                            "h8h5",
                            "e7f7",
                            "e7d6",
                            "e7c5",
                            "e7f8",
                            "e7d8",
                            "g7h6",
                            "g7f8",
                            "a6b7",
                            "a6c8",
                            "a6b5",
                            "a6c4",
                            "a6d3",
                            "a6e2",
                            "b6a4",
                            "b6c8",
                            "b6d5",
                            "b6c4",
                            "f6e4",
                            "f6g8",
                            "f6d5",
                            "f6h7",
                            "f6h5",
                            "f6g4"
                        ]
                    }
                ],
                all_possible_moves_test_acc(TestData);
            _ -> do_nothing  % ?DO_NON_EVOLUTIVE_TESTS
        end
    end
    ].
        
%% -----------------------------------------------------------------------------
string_to_position_test_() ->
    [
    fun() ->
        TestPositions = [
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            "4k3/8/R7/8/7b/8/3P4/7K b - - 33 32",
            "rn2k2r/pppppppp/8/8/1P1P1P1P/8/P1P1P1P1/R3K2R b Kq e3 3 12",
            "r3k2r/pppppppp/8/8/1P1P1P1P/8/P1P1P1P1/RN2K2R w Qk e6 3 12", 
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"], 
        string_to_position_test_acc(TestPositions)
    end
    ].
    
string_to_position_test_acc([]) -> true;
string_to_position_test_acc(Positions) ->
    [Test | Remaining] = Positions,
    Position = string_to_position(Test),
    ?assert(position_to_string(Position) =:= Test),
    string_to_position_test_acc(Remaining).
    
position_to_string_1_test_() ->
    [
    fun() ->
        % Make sure that position_to_string() with 1 parameter removes the counter and clock
        String1 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        String2 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -", 
        Position = string_to_position(String1),
        ?assert(position_to_string(Position) =:= String1),
        ?assert(position_to_string_without_counters(Position) =:= String2)
    end
    ].

%% -----------------------------------------------------------------------------
opponent_test_() ->
    [
    fun() ->
        TestData = [
            {black, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"}, 
            {white, "4k3/8/R7/8/7b/8/3P4/7K b - - 33 32"},
            {white, "rn2k2r/pppppppp/8/8/1P1P1P1P/8/P1P1P1P1/R3K2R b Kq e3 3 12"},
            {black, "r3k2r/pppppppp/8/8/1P1P1P1P/8/P1P1P1P1/RN2K2R w Qk e6 3 12"}], 
            opponent_test_acc(TestData)
    end
    ].
        
opponent_test_acc([]) -> true;
opponent_test_acc(TestData) ->
    [{Opponent, Test} | Remaining] = TestData,
    Position = string_to_position(Test),
    ?assertEqual(Opponent, opponent_color(Position)),
    opponent_test_acc(Remaining).
    
%% -----------------------------------------------------------------------------
player_color_test() ->
    TestPositions = [
        {"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", white}, 
        {"rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1", black}, 
        {"4k3/8/R7/8/7b/8/3P4/7K b - - 33 32", black},
        {"4k3/8/R7/8/7b/8/3P4/7K w - - 33 32", white}],
        
    player_color_test_acc(TestPositions).

player_color_test_acc([]) -> true;
player_color_test_acc(Positions) ->
    [{Test, ExpectedResult} | Remaining] = Positions,
    Position = string_to_position(Test),
    ?assert(player_color(Position) =:= ExpectedResult),
    player_color_test_acc(Remaining).
    
%% -----------------------------------------------------------------------------
all_possible_next_positions_test_() ->
    [
    fun() ->
        case ?DO_NON_EVOLUTIVE_TESTS of
            true ->
                TestData = [
                    {   % Specific position
                        "4k2R/8/8/8/8/8/8/4K3 b - - 1 1", 
                        [
                        % King moves   
                        "7R/5k2/8/8/8/8/8/4K3 w - - 2 2", 
                        "7R/4k3/8/8/8/8/8/4K3 w - - 2 2", 
                        "7R/3k4/8/8/8/8/8/4K3 w - - 2 2"
                        ]
                    },
                    {   % Specific position
                        "8/8/8/1Ppp3r/RK3p1k/8/4P1P1/8 w - c6 0 1", 
                        [
                        % King moves   
                        "8/8/8/1Ppp3r/R4p1k/K7/4P1P1/8 b - - 1 1", 
                        "8/8/8/1Ppp3r/R4p1k/1K6/4P1P1/8 b - - 1 1", 
                        "8/8/8/1Ppp3r/R4p1k/2K5/4P1P1/8 b - - 1 1", 
                        "8/8/8/KPpp3r/R4p1k/8/4P1P1/8 b - - 1 1", 
                        "8/8/8/1PKp3r/R4p1k/8/4P1P1/8 b - - 0 1", 
                        % Pawns move
                        "8/8/2P5/3p3r/RK3p1k/8/4P1P1/8 b - - 0 1"
                        ]
                    },
                    {   % Specific position
                        "r3k2r/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 1 1", 
                        [
                        % Rooks move
                        "1r2k2r/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQk - 2 2", 
                        "2r1k2r/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQk - 2 2", 
                        "3rk2r/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQk - 2 2", 
                        
                        "r3k1r1/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        "r3kr2/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        "r3k3/p1ppqpbr/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        "r3k3/p1ppqpb1/bn2pnpr/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        "r3k3/p1ppqpb1/bn2pnp1/3P3r/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        "r3k3/p1ppqpb1/bn2pnp1/3P4/1p2P1Nr/2N2Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        % King moves
                        "r2k3r/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQ - 2 2", 
                        "r4k1r/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQ - 2 2", 
                        % Castling
                        "2kr3r/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQ - 2 2", 
                        "r4rk1/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQ - 2 2", 
                        % Queen moves
                        "r2qk2r/p1pp1pb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3kq1r/p1pp1pb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1pp1pb1/bn1qpnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1pp1pb1/bn2pnp1/2qP4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        % Bishops move
                        "r1b1k2r/p1ppqpb1/1n2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/pbppqpb1/1n2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/1b1P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3P4/1pb1P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3P4/1p2P1N1/2Nb1Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBbPPP/R3K2R w KQkq - 0 2", 
                        "r3kb1r/p1ppqp2/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqp2/bn2pnpb/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        % Knights move
                        "r1n1k2r/p1ppqpb1/b3pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3n4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3P4/1pn1P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3P4/np2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k1nr/p1ppqpb1/bn2p1p1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpbn/bn2p1p1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3P3n/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3P4/1p2P1n1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3P4/1p2n1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3n4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        % Pawns move
                        "r3k2r/p1ppqpb1/bn2pnp1/3P4/4P1N1/1pN2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pnp1/3P4/4P1N1/2p2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        
                        "r3k2r/p2pqpb1/bnp1pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p2pqpb1/bn2pnp1/2pP4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq c6 0 2", 
                        
                        "r3k2r/p1p1qpb1/bn1ppnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2",
                        
                        "r3k2r/p1ppqpb1/bn3np1/3p4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn3np1/3Pp3/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2",
                        
                        "r3k2r/p1ppqpb1/bn2pn2/3P2p1/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        
                        "r3k2r/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q2/PPPBBPpP/R3K2R w KQkq - 0 2" 
                        ]
                    },
                    {   % Specific position
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 
                        [
                        % Rooks move
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/1R2K2R b Kkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/2R1K2R b Kkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/3RK2R b Kkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K1R1 b Qkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3KR2 b Qkq - 1 1",
                        % King moves
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R2K3R b kq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4K1R b kq - 1 1",
                        % Castling
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/2KR3R b kq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 b kq - 1 1",
                        % Queen moves
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N1Q2p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2NQ3p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N3Qp/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N4Q/PPPBBPPP/R3K2R b KQkq - 0 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2PQ2/2N4p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PNQ2/1p2P3/2N4p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pQp1/3PN3/1p2P3/2N4p/PPPBBPPP/R3K2R b KQkq - 0 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P1Q1/2N4p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN2Q/1p2P3/2N4p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        % Bishops move
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPP1BPPP/R1B1K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N1BQ1p/PPP1BPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2PB2/2N2Q1p/PPP1BPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN1B1/1p2P3/2N2Q1p/PPP1BPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnpB/3PN3/1p2P3/2N2Q1p/PPP1BPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3KB1R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R2BK2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2NB1Q1p/PPPB1PPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1pB1P3/2N2Q1p/PPPB1PPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/1B1PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R b KQkq - 0 1",
                        % Knights move
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/5Q1p/PPPBBPPP/RN2K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/5Q1p/PPPBBPPP/R2NK2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/Np2P3/5Q1p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3P4/1p2P3/2NN1Q1p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3P4/1pN1P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3P4/1p2P1N1/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 1 1",
                        "r3k2r/p1ppqpb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1",
                        "r3k2r/p1pNqpb1/bn2pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1",
                        "r3k2r/p1ppqNb1/bn2pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1",
                        % Pawns move
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/P1N2Q1p/1PPBBPPP/R3K2R b KQkq - 0 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R b KQkq a3 0 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/1PN2Q1p/P1PBBPPP/R3K2R b KQkq - 0 1",
                        "r3k2r/p1ppqpb1/bn1Ppnp1/4N3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1",
                        "r3k2r/p1ppqpb1/bn2Pnp1/4N3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2QPp/PPPBBP1P/R3K2R b KQkq - 0 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P1P1/2N2Q1p/PPPBBP1P/R3K2R b KQkq g3 0 1",
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1P/PPPBBP1P/R3K2R b KQkq - 0 1"
                        ]
                    },
                    {   % Specific position
                        "r3k2r/p1ppqpb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1", 
                        [
                        % Rooks move
                        "1r2k2r/p1ppqpb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQk - 1 2", 
                        "2r1k2r/p1ppqpb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQk - 1 2", 
                        "3rk2r/p1ppqpb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQk - 1 2", 
            
                        "r3k1r1/p1ppqpb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3kr2/p1ppqpb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpbr/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bn2pnNr/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bn2pnN1/3P3r/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bn2pnN1/3P4/1p2P2r/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        % King moves
                        "r2k3r/p1ppqpb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQ - 1 2", 
                        % Castling
                        "2kr3r/p1ppqpb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQ - 1 2", 
                        % Queen moves
                        "r2qk2r/p1pp1pb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3kq1r/p1pp1pb1/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1pp1pb1/bn1qpnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1pp1pb1/bn2pnN1/2qP4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        % Bishops move
                        "r3k2r/pbppqpb1/1n2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r1b1k2r/p1ppqpb1/1n2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnN1/1b1P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnN1/3P4/1pb1P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnN1/3P4/1p2P3/2Nb1Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnN1/3P4/1p2P3/2N2Q1p/PPPBbPPP/R3K2R w KQkq - 0 2", 
                        "r3kb1r/p1ppqp2/bn2pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqp2/bn2pnNb/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        % Knights move   3
                        "r1n1k2r/p1ppqpb1/b3pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/b3pnN1/3n4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/b3pnN1/3P4/1pn1P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/b3pnN1/3P4/np2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k1nr/p1ppqpb1/bn2p1N1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpbn/bn2p1N1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bn2p1N1/3P3n/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bn2p1N1/3P4/1p2P1n1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bn2p1N1/3P4/1p2n3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2p1N1/3n4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                         % Pawns move
                        "r3k2r/p1ppqpb1/bn2pnN1/3P4/4P3/1pN2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pnN1/3P4/4P3/2p2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p2pqpb1/bnp1pnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p2pqpb1/bn2pnN1/2pP4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq c6 0 2",
                        
                        "r3k2r/p1p1qpb1/bn1ppnN1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2",
                        
                        "r3k2r/p1ppqpb1/bn3nN1/3p4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn3nN1/3Pp3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        
                        "r3k2r/p1ppq1b1/bn2pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2",
                        
                        "r3k2r/p1ppqpb1/bn2pnN1/3P4/1p2P3/2N2Q2/PPPBBPpP/R3K2R w KQkq - 0 2" 
                        ]
                    },
                    {   % Specific position
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 b kq - 1 1",
                        [
                        % Rooks move
                        "1r2k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w k - 2 2", 
                        "2r1k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w k - 2 2", 
                        "3rk2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w k - 2 2", 
                        "r3k1r1/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w q - 2 2", 
                        "r3kr2/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w q - 2 2", 
                        "r3k3/p1ppqpbr/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w q - 2 2", 
                        "r3k3/p1ppqpb1/bn2pnpr/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w q - 2 2", 
                        "r3k3/p1ppqpb1/bn2pnp1/3PN2r/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w q - 2 2", 
                        "r3k3/p1ppqpb1/bn2pnp1/3PN3/1p2P2r/2N2Q1p/PPPBBPPP/R4RK1 w q - 2 2", 
                        % King moves
                        "r2k3r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w - - 2 2", 
                        "r4k1r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w - - 2 2", 
                        % Castling
                        "2kr3r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w - - 2 2", 
                        "r4rk1/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w - - 2 2", 
                        % Queen moves
                        "r2qk2r/p1pp1pb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3kq1r/p1pp1pb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1pp1pb1/bn1qpnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1pp1pb1/bn2pnp1/2qPN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        % Bishops move
                        "r3k2r/pbppqpb1/1n2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r1b1k2r/p1ppqpb1/1n2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/1b1PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3PN3/1pb1P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3PN3/1p2P3/2Nb1Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3PN3/1p2P3/2N2Q1p/PPPBbPPP/R4RK1 w kq - 0 2", 
                        "r3kb1r/p1ppqp2/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqp2/bn2pnpb/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        % Knights move
                        "r1n1k2r/p1ppqpb1/b3pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3nN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 0 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3PN3/1pn1P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3PN3/np2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k1nr/p1ppqpb1/bn2p1p1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqpbn/bn2p1p1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3PN2n/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3PN3/1p2P1n1/2N2Q1p/PPPBBPPP/R4RK1 w kq - 2 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3PN3/1p2n3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3nN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 0 2", 
                        % Pawns move
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/4P3/2p2Q1p/PPPBBPPP/R4RK1 w kq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/4P3/1pN2Q1p/PPPBBPPP/R4RK1 w kq - 0 2", 
                        "r3k2r/p2pqpb1/bnp1pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 0 2", 
                        "r3k2r/p2pqpb1/bn2pnp1/2pPN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq c6 0 2", 
                        "r3k2r/p1p1qpb1/bn1ppnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 0 2", 
                        "r3k2r/p1ppqpb1/bn3np1/3pN3/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pn2/3PN1p1/1p2P3/2N2Q1p/PPPBBPPP/R4RK1 w kq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q2/PPPBBPpP/R4RK1 w kq - 0 2"
                        ]
                    },
                    {   % Specific position
                        "r3k2r/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R b KQkq - 1 1", 
                        [
                        % Rooks move
                        "1r2k2r/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQk - 2 2", 
                        "2r1k2r/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQk - 2 2", 
                        "3rk2r/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQk - 2 2", 
            
                        "r3k1r1/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        "r3kr2/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        "r3k3/p1ppqpbr/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        "r3k3/p1ppqpb1/bn2pnpr/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        "r3k3/p1ppqpb1/bn2pnp1/1N1PN2r/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        "r3k3/p1ppqpb1/bn2pnp1/1N1PN3/1p2P2r/5Q1p/PPPBBPPP/R3K2R w KQq - 2 2", 
                        % King moves
                        "r2k3r/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQ - 2 2", 
                        "r4k1r/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQ - 2 2", 
                        % Castling
                        "2kr3r/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQ - 2 2", 
                        "r4rk1/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQ - 2 2", 
                        % Queen moves
                        "r2qk2r/p1pp1pb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3kq1r/p1pp1pb1/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1pp1pb1/bn1qpnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1pp1pb1/bn2pnp1/1NqPN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        % Bishops move
                        "r3k2r/pbppqpb1/1n2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r1b1k2r/p1ppqpb1/1n2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/1b1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3kb1r/p1ppqp2/bn2pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqp2/bn2pnpb/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        % Knights move
                        "r1n1k2r/p1ppqpb1/b3pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/1N1nN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/1N1PN3/1pn1P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/1N1PN3/np2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
            
                        "r3k1nr/p1ppqpb1/bn2p1p1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpbn/bn2p1p1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/1N1PN2n/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/1N1PN3/1p2P1n1/5Q1p/PPPBBPPP/R3K2R w KQkq - 2 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/1N1PN3/1p2n3/5Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/1N1nN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        % Pawns move
                        "r3k2r/p1ppqpb1/bn2pnp1/1N1PN3/4P3/1p3Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
            
                        "r3k2r/p2pqpb1/bnp1pnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p2pqpb1/bn2pnp1/1NpPN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq c6 0 2", 
            
                        "r3k2r/p1p1qpb1/bn1ppnp1/1N1PN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
            
                        "r3k2r/p1ppqpb1/bn3np1/1N1pN3/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
            
                        "r3k2r/p1ppqpb1/bn2pn2/1N1PN1p1/1p2P3/5Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
            
                        "r3k2r/p1ppqpb1/bn2pnp1/1N1PN3/1p2P3/5Q2/PPPBBPpP/R3K2R w KQkq - 0 2"
                        ]
                    },
                    {   % Specific position
                        "r3k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R b KQkq - 0 1", 
                        [
                        % Rooks move
                        "1r2k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQk - 1 2", 
                        "2r1k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQk - 1 2", 
                        "3rk2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQk - 1 2", 
                        "r3k1r1/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQq - 1 2", 
                        "r3kr2/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpbr/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/Bn2pnpr/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/Bn2pnp1/3PN2r/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/Bn2pnp1/3PN3/1p2P2r/2N2Q1p/PPPB1PPP/R3K2R w KQq - 1 2", 
                        % King moves
                        "r2k3r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQ - 1 2", 
                        "r4k1r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQ - 1 2", 
                        % Castling
                        "r4rk1/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQ - 1 2", 
                        % Queen moves
                        "r2qk2r/p1pp1pb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3kq1r/p1pp1pb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1pp1pb1/Bn1qpnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1pp1pb1/Bn2pnp1/2qPN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        % Bishops move
                        "r3kb1r/p1ppqp2/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqp2/Bn2pnpb/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        % Knights move
                        "r1n1k2r/p1ppqpb1/B3pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/B3pnp1/3nN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/B3pnp1/3PN3/1pn1P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/B3pnp1/3PN3/np2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3k1nr/p1ppqpb1/Bn2p1p1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpbn/Bn2p1p1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/Bn2p1p1/3PN2n/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/Bn2p1p1/3nN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/Bn2p1p1/3PN3/1p2P1n1/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/Bn2p1p1/3PN3/1p2n3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 0 2", 
                        % Pawns move
                        "r3k2r/p1ppqpb1/Bn2pnp1/3PN3/4P3/1pN2Q1p/PPPB1PPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/Bn2pnp1/3PN3/4P3/2p2Q1p/PPPB1PPP/R3K2R w KQkq - 0 2",
                        "r3k2r/p2pqpb1/Bnp1pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p2pqpb1/Bn2pnp1/2pPN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq c6 0 2", 
                        "r3k2r/p1p1qpb1/Bn1ppnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/Bn3np1/3pN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 0 2",
                        "r3k2r/p1ppqpb1/Bn2pn2/3PN1p1/1p2P3/2N2Q1p/PPPB1PPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q2/PPPB1PpP/R3K2R w KQkq - 0 2"
                        ]
                    },
                    {   % Specific position
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1", 
                        [
                        % Rooks move
                        "1r2k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQk - 1 2", 
                        "2r1k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQk - 1 2", 
                        "3rk2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQk - 1 2", 
                        "r3k1r1/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3kr2/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpbr/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bn2pnpr/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bn2pnp1/3PN2r/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bn2pnp1/3PN3/1p2P2r/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        % King moves
                        "r2k3r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQ - 1 2", 
                        "r4k1r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQ - 1 2", 
                        % Castling
                        "2kr3r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQ - 1 2", 
                        "r4rk1/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQ - 1 2", 
                        % Queen moves
                        "r2qk2r/p1pp1pb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3kq1r/p1pp1pb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1pp1pb1/bn1qpnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1pp1pb1/bn2pnp1/2qPN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        % Bishops move
                        "r3k2r/pbppqpb1/1n2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r1b1k2r/p1ppqpb1/1n2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/1b1PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3PN3/1pb1P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3PN3/1p2P3/2Nb1Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3PN3/1p2P3/2N2Q1p/PPPBbPPP/R3K2R w KQkq - 0 2", 
                        "r3kb1r/p1ppqp2/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqp2/bn2pnpb/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        % Knights move
                        "r1n1k2r/p1ppqpb1/b3pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3nN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3PN3/1pn1P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3PN3/np2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k1nr/p1ppqpb1/bn2p1p1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpbn/bn2p1p1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3PN2n/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3PN3/1p2P1n1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3PN3/1p2n3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3nN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        % Pawns move
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/4P3/2p2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/4P3/1pN2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p2pqpb1/bnp1pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p2pqpb1/bn2pnp1/2pPN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq c6 0 2", 
                        "r3k2r/p1p1qpb1/bn1ppnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn3np1/3pN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pn2/3PN1p1/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q2/PPPBBPpP/R3K2R w KQkq - 0 2"
                        ]
                    },
                    {   % Specific position
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R b KQkq a3 0 1", 
                        [
                        % Rooks move
                        "1r2k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQk - 1 2", 
                        "2r1k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQk - 1 2", 
                        "3rk2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQk - 1 2", 
                        "r3k1r1/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQq - 1 2", 
                        "r3kr2/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpbr/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bn2pnpr/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bn2pnp1/3PN2r/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bn2pnp1/3PN3/Pp2P2r/2N2Q1p/1PPBBPPP/R3K2R w KQq - 1 2", 
                        % King moves
                        "r2k3r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQ - 1 2", 
                        "r4k1r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQ - 1 2", 
                        % Castling
                        "2kr3r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQ - 1 2", 
                        "r4rk1/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQ - 1 2", 
                        % Queen moves
                        "r2qk2r/p1pp1pb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3kq1r/p1pp1pb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1pp1pb1/bn1qpnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1pp1pb1/bn2pnp1/2qPN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        % Bishops move
                        "r3k2r/pbppqpb1/1n2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r1b1k2r/p1ppqpb1/1n2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/1b1PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3PN3/Ppb1P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3PN3/Pp2P3/2Nb1Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1n2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBbPPP/R3K2R w KQkq - 0 2", 
                        "r3kb1r/p1ppqp2/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqp2/bn2pnpb/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        % Knights move
                        "r1n1k2r/p1ppqpb1/b3pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3nN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3PN3/Ppn1P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/b3pnp1/3PN3/np2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k1nr/p1ppqpb1/bn2p1p1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpbn/bn2p1p1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3PN2n/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3PN3/Pp2P1n1/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3PN3/Pp2n3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2p1p1/3nN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        % Pawns move
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/P3P3/2p2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/P3P3/1pN2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/4P3/p1N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p2pqpb1/bnp1pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p2pqpb1/bn2pnp1/2pPN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq c6 0 2", 
                        "r3k2r/p1p1qpb1/bn1ppnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn3np1/3pN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pn2/3PN1p1/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q2/1PPBBPpP/R3K2R w KQkq - 0 2"
                        ]
                    },
                    {   % Specific position
                        "r3k2r/p1ppqpb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1", 
                        [
                        % Rooks move
                        "1r2k2r/p1ppqpb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQk - 1 2", 
                        "2r1k2r/p1ppqpb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQk - 1 2", 
                        "3rk2r/p1ppqpb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQk - 1 2", 
                        "r3k1r1/p1ppqpb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3kr2/p1ppqpb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpbr/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bnN1pnpr/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bnN1pnp1/3P3r/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        "r3k3/p1ppqpb1/bnN1pnp1/3P4/1p2P2r/2N2Q1p/PPPBBPPP/R3K2R w KQq - 1 2", 
                        % King moves
                        "r4k1r/p1ppqpb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQ - 1 2", 
                        % Castling
                        "r4rk1/p1ppqpb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQ - 1 2", 
                        % Queen moves
                        "r2qk2r/p1pp1pb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3kq1r/p1pp1pb1/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1pp1pb1/bnNqpnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1pp1pb1/bnN1pnp1/2qP4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        % Bishops move
                        "r3k2r/pbppqpb1/1nN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r1b1k2r/p1ppqpb1/1nN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1nN1pnp1/1b1P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1nN1pnp1/3P4/1pb1P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1nN1pnp1/3P4/1p2P3/2Nb1Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/1nN1pnp1/3P4/1p2P3/2N2Q1p/PPPBbPPP/R3K2R w KQkq - 0 2", 
                        "r3kb1r/p1ppqp2/bnN1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqp2/bnN1pnpb/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        % Knights move
                        "r1n1k2r/p1ppqpb1/b1N1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/b1N1pnp1/3n4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/b1N1pnp1/3P4/1pn1P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/b1N1pnp1/3P4/np2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k1nr/p1ppqpb1/bnN1p1p1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpbn/bnN1p1p1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bnN1p1p1/3P3n/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bnN1p1p1/3P4/1p2P1n1/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 1 2", 
                        "r3k2r/p1ppqpb1/bnN1p1p1/3P4/1p2n3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bnN1p1p1/3n4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        % Pawns move
                        "r3k2r/p1ppqpb1/bnN1pnp1/3P4/4P3/2p2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bnN1pnp1/3P4/4P3/1pN2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        
                        "r3k2r/p1p1qpb1/bnp1pnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1p1qpb1/bnNppnp1/3P4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
            
                        "r3k2r/p1ppqpb1/bnN2np1/3p4/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bnN2np1/3Pp3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        
                        "r3k2r/p1ppqpb1/bnN1pn2/3P2p1/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 2", 
                        "r3k2r/p1ppqpb1/bnN1pnp1/3P4/1p2P3/2N2Q2/PPPBBPpP/R3K2R w KQkq - 0 2"
                        ]
                    },
                    {   % Specific position
                        "K7/8/8/3Q4/4q3/8/8/7k w - - 0 1", 
                        [
                        "K7/8/8/8/4Q3/8/8/7k b - - 0 1", 
                        "K7/8/2Q5/8/4q3/8/8/7k b - - 1 1", 
                        "K7/1Q6/8/8/4q3/8/8/7k b - - 1 1", 
                        "1K6/8/8/3Q4/4q3/8/8/7k b - - 1 1", 
                        "8/1K6/8/3Q4/4q3/8/8/7k b - - 1 1", 
                        "8/K7/8/3Q4/4q3/8/8/7k b - - 1 1"
                        ]
                    }, 
                    {   %White rook and king play
                        "r3k3/8/8/8/8/8/8/4K2R w - - 0 20", 
                        [
                        "r3k3/8/8/8/8/8/8/4K1R1 b - - 1 20", 
                        "r3k3/8/8/8/8/8/8/4KR2 b - - 1 20", 
                        "r3k3/8/8/8/8/8/7R/4K3 b - - 1 20", 
                        "r3k3/8/8/8/8/7R/8/4K3 b - - 1 20", 
                        "r3k3/8/8/8/7R/8/8/4K3 b - - 1 20", 
                        "r3k3/8/8/7R/8/8/8/4K3 b - - 1 20", 
                        "r3k3/8/7R/8/8/8/8/4K3 b - - 1 20", 
                        "r3k3/7R/8/8/8/8/8/4K3 b - - 1 20", 
                        "r3k2R/8/8/8/8/8/8/4K3 b - - 1 20", 
                        "r3k3/8/8/8/8/8/8/3K3R b - - 1 20", 
                        "r3k3/8/8/8/8/8/8/5K1R b - - 1 20", 
                        "r3k3/8/8/8/8/8/3K4/7R b - - 1 20", 
                        "r3k3/8/8/8/8/8/4K3/7R b - - 1 20", 
                        "r3k3/8/8/8/8/8/5K2/7R b - - 1 20"
                        ]
                    }, 
                    {   %White knight and king play
                        "1n2k3/8/8/8/8/8/8/4K1N1 w - - 0 20", 
                        [
                        "1n2k3/8/8/8/8/7N/8/4K3 b - - 1 20", 
                        "1n2k3/8/8/8/8/5N2/8/4K3 b - - 1 20", 
                        "1n2k3/8/8/8/8/8/4N3/4K3 b - - 1 20", 
                        "1n2k3/8/8/8/8/8/8/3K2N1 b - - 1 20", 
                        "1n2k3/8/8/8/8/8/8/5KN1 b - - 1 20", 
                        "1n2k3/8/8/8/8/8/3K4/6N1 b - - 1 20", 
                        "1n2k3/8/8/8/8/8/4K3/6N1 b - - 1 20", 
                        "1n2k3/8/8/8/8/8/5K2/6N1 b - - 1 20"
                        ]
                    },
                    {   %White bishop and king play
                        "2b1k3/8/8/8/8/8/8/4KB2 w - - 0 20", 
                        [
                        "2b1k3/8/8/8/8/8/6B1/4K3 b - - 1 20", 
                        "2b1k3/8/8/8/8/7B/8/4K3 b - - 1 20", 
                        "2b1k3/8/8/8/8/8/4B3/4K3 b - - 1 20", 
                        "2b1k3/8/8/8/8/3B4/8/4K3 b - - 1 20", 
                        "2b1k3/8/8/8/2B5/8/8/4K3 b - - 1 20", 
                        "2b1k3/8/8/1B6/8/8/8/4K3 b - - 1 20", 
                        "2b1k3/8/B7/8/8/8/8/4K3 b - - 1 20", 
                        "2b1k3/8/8/8/8/8/8/3K1B2 b - - 1 20", 
                        "2b1k3/8/8/8/8/8/3K4/5B2 b - - 1 20", 
                        "2b1k3/8/8/8/8/8/4K3/5B2 b - - 1 20", 
                        "2b1k3/8/8/8/8/8/5K2/5B2 b - - 1 20"
                        ]
                    },
                    {   %White queen and king play
                        "3qk3/8/8/8/8/8/8/3QK3 w - - 0 20", 
                        [
                        "3qk3/8/8/8/8/8/8/2Q1K3 b - - 1 20",
                        "3qk3/8/8/8/8/8/8/1Q2K3 b - - 1 20",
                        "3qk3/8/8/8/8/8/8/Q3K3 b - - 1 20",
                        "3qk3/8/8/8/8/8/3Q4/4K3 b - - 1 20",
                        "3qk3/8/8/8/8/3Q4/8/4K3 b - - 1 20",
                        "3qk3/8/8/8/3Q4/8/8/4K3 b - - 1 20",
                        "3qk3/8/8/3Q4/8/8/8/4K3 b - - 1 20",
                        "3qk3/8/3Q4/8/8/8/8/4K3 b - - 1 20",
                        "3qk3/3Q4/8/8/8/8/8/4K3 b - - 1 20",
                        "3Qk3/8/8/8/8/8/8/4K3 b - - 0 20",
                        "3qk3/8/8/8/8/8/4Q3/4K3 b - - 1 20", 
                        "3qk3/8/8/8/8/5Q2/8/4K3 b - - 1 20", 
                        "3qk3/8/8/8/6Q1/8/8/4K3 b - - 1 20", 
                        "3qk3/8/8/7Q/8/8/8/4K3 b - - 1 20", 
                        "3qk3/8/8/8/8/8/2Q5/4K3 b - - 1 20", 
                        "3qk3/8/8/8/8/1Q6/8/4K3 b - - 1 20", 
                        "3qk3/8/8/8/Q7/8/8/4K3 b - - 1 20", 
                        "3qk3/8/8/8/8/8/8/3Q1K2 b - - 1 20", 
                        "3qk3/8/8/8/8/8/4K3/3Q4 b - - 1 20", 
                        "3qk3/8/8/8/8/8/5K2/3Q4 b - - 1 20"
                        ]
                    },
                    {   %White pawn on 2nd row and king play
                        "4k3/8/8/8/8/8/4P3/4K3 w - - 0 20", 
                        [
                        "4k3/8/8/8/8/4P3/8/4K3 b - - 0 20", 
                        "4k3/8/8/8/4P3/8/8/4K3 b - e3 0 20", 
                        "4k3/8/8/8/8/8/4P3/5K2 b - - 1 20",
                        "4k3/8/8/8/8/8/4P3/3K4 b - - 1 20",
                        "4k3/8/8/8/8/8/4PK2/8 b - - 1 20",
                        "4k3/8/8/8/8/8/3KP3/8 b - - 1 20"
                        ]
                    },
                    {   %White pawn may take "en passant", or king plays
                        "4k3/8/3p4/4Pp2/8/8/8/4K3 w - f6 0 20", 
                        [
                        "4k3/8/3pP3/5p2/8/8/8/4K3 b - - 0 20", 
                        "4k3/8/3P4/5p2/8/8/8/4K3 b - - 0 20", 
                        "4k3/8/3p1P2/8/8/8/8/4K3 b - - 0 20", 
                        "4k3/8/3p4/4Pp2/8/8/8/5K2 b - - 1 20",
                        "4k3/8/3p4/4Pp2/8/8/8/3K4 b - - 1 20",
                        "4k3/8/3p4/4Pp2/8/8/5K2/8 b - - 1 20",
                        "4k3/8/3p4/4Pp2/8/8/4K3/8 b - - 1 20",
                        "4k3/8/3p4/4Pp2/8/8/3K4/8 b - - 1 20"
                        ]
                    },
                    {   %White pawn may get promoted, or king plays
                        "7k/3P4/8/8/8/8/8/4K3 w - - 0 20", 
                        [
                        "3Q3k/8/8/8/8/8/8/4K3 b - - 0 20", 
                        "3R3k/8/8/8/8/8/8/4K3 b - - 0 20", 
                        "3N3k/8/8/8/8/8/8/4K3 b - - 0 20", 
                        "3B3k/8/8/8/8/8/8/4K3 b - - 0 20", 
                        "7k/3P4/8/8/8/8/8/5K2 b - - 1 20",
                        "7k/3P4/8/8/8/8/8/3K4 b - - 1 20",
                        "7k/3P4/8/8/8/8/5K2/8 b - - 1 20",
                        "7k/3P4/8/8/8/8/4K3/8 b - - 1 20",
                        "7k/3P4/8/8/8/8/3K4/8 b - - 1 20"
                        ]
                    },
                    {   %White king cannot castle queenside
                        "7k/8/8/8/8/8/8/R3K3 w - - 0 20", 
                        [
                        "7k/8/8/8/8/8/8/1R2K3 b - - 1 20", 
                        "7k/8/8/8/8/8/8/2R1K3 b - - 1 20", 
                        "7k/8/8/8/8/8/8/3RK3 b - - 1 20", 
                        "7k/8/8/8/8/8/R7/4K3 b - - 1 20", 
                        "7k/8/8/8/8/R7/8/4K3 b - - 1 20", 
                        "7k/8/8/8/R7/8/8/4K3 b - - 1 20", 
                        "7k/8/8/R7/8/8/8/4K3 b - - 1 20", 
                        "7k/8/R7/8/8/8/8/4K3 b - - 1 20", 
                        "7k/R7/8/8/8/8/8/4K3 b - - 1 20", 
                        "R6k/8/8/8/8/8/8/4K3 b - - 1 20", 
                        "7k/8/8/8/8/8/8/R4K2 b - - 1 20",
                        "7k/8/8/8/8/8/8/R2K4 b - - 1 20",
                        "7k/8/8/8/8/8/5K2/R7 b - - 1 20",
                        "7k/8/8/8/8/8/4K3/R7 b - - 1 20",
                        "7k/8/8/8/8/8/3K4/R7 b - - 1 20"
                        ]
                    },
                    {   %White king can castle queenside
                        "7k/8/8/8/8/8/8/R3K3 w Q - 0 20", 
                        [
                        "7k/8/8/8/8/8/8/2KR4 b - - 1 20", 
                        "7k/8/8/8/8/8/8/1R2K3 b - - 1 20", 
                        "7k/8/8/8/8/8/8/2R1K3 b - - 1 20", 
                        "7k/8/8/8/8/8/8/3RK3 b - - 1 20", 
                        "7k/8/8/8/8/8/R7/4K3 b - - 1 20", 
                        "7k/8/8/8/8/R7/8/4K3 b - - 1 20", 
                        "7k/8/8/8/R7/8/8/4K3 b - - 1 20", 
                        "7k/8/8/R7/8/8/8/4K3 b - - 1 20", 
                        "7k/8/R7/8/8/8/8/4K3 b - - 1 20", 
                        "7k/R7/8/8/8/8/8/4K3 b - - 1 20", 
                        "R6k/8/8/8/8/8/8/4K3 b - - 1 20", 
                        "7k/8/8/8/8/8/8/R4K2 b - - 1 20",
                        "7k/8/8/8/8/8/8/R2K4 b - - 1 20",
                        "7k/8/8/8/8/8/5K2/R7 b - - 1 20",
                        "7k/8/8/8/8/8/4K3/R7 b - - 1 20",
                        "7k/8/8/8/8/8/3K4/R7 b - - 1 20"
                        ]
                    },
                    {   %White king cannot castle kingside
                        "k7/8/8/8/8/8/8/4K2R w - - 0 20", 
                        [
                        "k7/8/8/8/8/8/8/4K1R1 b - - 1 20", 
                        "k7/8/8/8/8/8/8/4KR2 b - - 1 20", 
                        "k7/8/8/8/8/8/7R/4K3 b - - 1 20", 
                        "k7/8/8/8/8/7R/8/4K3 b - - 1 20", 
                        "k7/8/8/8/7R/8/8/4K3 b - - 1 20", 
                        "k7/8/8/7R/8/8/8/4K3 b - - 1 20", 
                        "k7/8/7R/8/8/8/8/4K3 b - - 1 20", 
                        "k7/7R/8/8/8/8/8/4K3 b - - 1 20", 
                        "k6R/8/8/8/8/8/8/4K3 b - - 1 20", 
                        "k7/8/8/8/8/8/8/5K1R b - - 1 20",
                        "k7/8/8/8/8/8/8/3K3R b - - 1 20",
                        "k7/8/8/8/8/8/5K2/7R b - - 1 20",
                        "k7/8/8/8/8/8/4K3/7R b - - 1 20",
                        "k7/8/8/8/8/8/3K4/7R b - - 1 20"
                        ]
                    },
                    {   %White king can castle kingside
                        "k7/8/8/8/8/8/8/4K2R w K - 0 20", 
                        [
                        "k7/8/8/8/8/8/8/5RK1 b - - 1 20", 
                        "k7/8/8/8/8/8/8/4K1R1 b - - 1 20", 
                        "k7/8/8/8/8/8/8/4KR2 b - - 1 20", 
                        "k7/8/8/8/8/8/7R/4K3 b - - 1 20", 
                        "k7/8/8/8/8/7R/8/4K3 b - - 1 20", 
                        "k7/8/8/8/7R/8/8/4K3 b - - 1 20", 
                        "k7/8/8/7R/8/8/8/4K3 b - - 1 20", 
                        "k7/8/7R/8/8/8/8/4K3 b - - 1 20", 
                        "k7/7R/8/8/8/8/8/4K3 b - - 1 20", 
                        "k6R/8/8/8/8/8/8/4K3 b - - 1 20", 
                        "k7/8/8/8/8/8/8/5K1R b - - 1 20",
                        "k7/8/8/8/8/8/8/3K3R b - - 1 20",
                        "k7/8/8/8/8/8/5K2/7R b - - 1 20",
                        "k7/8/8/8/8/8/4K3/7R b - - 1 20",
                        "k7/8/8/8/8/8/3K4/7R b - - 1 20"
                        ]
                    },
                    {   %Update castling information after king or rook move
                        "4k3/8/8/8/8/8/8/R3K2R w KQ - 0 20", 
                        [
                        "4k3/8/8/8/8/8/8/R2K3R b - - 1 20",
                        "4k3/8/8/8/8/8/8/R4K1R b - - 1 20",
                        "4k3/8/8/8/8/8/3K4/R6R b - - 1 20",
                        "4k3/8/8/8/8/8/4K3/R6R b - - 1 20",
                        "4k3/8/8/8/8/8/5K2/R6R b - - 1 20",
            
                        "4k3/8/8/8/8/8/8/2KR3R b - - 1 20",
                        "4k3/8/8/8/8/8/8/R4RK1 b - - 1 20",
                            
                        "4k3/8/8/8/8/8/8/1R2K2R b K - 1 20",
                        "4k3/8/8/8/8/8/8/2R1K2R b K - 1 20",
                        "4k3/8/8/8/8/8/8/3RK2R b K - 1 20",
                        "4k3/8/8/8/8/8/R7/4K2R b K - 1 20",
                        "4k3/8/8/8/8/R7/8/4K2R b K - 1 20",
                        "4k3/8/8/8/R7/8/8/4K2R b K - 1 20",
                        "4k3/8/8/R7/8/8/8/4K2R b K - 1 20",
                        "4k3/8/R7/8/8/8/8/4K2R b K - 1 20",
                        "4k3/R7/8/8/8/8/8/4K2R b K - 1 20",
                        "R3k3/8/8/8/8/8/8/4K2R b K - 1 20",
                            
                        "4k3/8/8/8/8/8/8/R3K1R1 b Q - 1 20",
                        "4k3/8/8/8/8/8/8/R3KR2 b Q - 1 20",
                        "4k3/8/8/8/8/8/7R/R3K3 b Q - 1 20",
                        "4k3/8/8/8/8/7R/8/R3K3 b Q - 1 20",
                        "4k3/8/8/8/7R/8/8/R3K3 b Q - 1 20",
                        "4k3/8/8/7R/8/8/8/R3K3 b Q - 1 20",
                        "4k3/8/7R/8/8/8/8/R3K3 b Q - 1 20",
                        "4k3/7R/8/8/8/8/8/R3K3 b Q - 1 20",
                        "4k2R/8/8/8/8/8/8/R3K3 b Q - 1 20" 
                        ]
                    },
                    {   %Start position
                    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
                        [
                        "rnbqkbnr/pppppppp/8/8/8/P7/1PPPPPPP/RNBQKBNR b KQkq - 0 1", 
                        "rnbqkbnr/pppppppp/8/8/8/1P6/P1PPPPPP/RNBQKBNR b KQkq - 0 1", 
                        "rnbqkbnr/pppppppp/8/8/8/2P5/PP1PPPPP/RNBQKBNR b KQkq - 0 1", 
                        "rnbqkbnr/pppppppp/8/8/8/3P4/PPP1PPPP/RNBQKBNR b KQkq - 0 1", 
                        "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1", 
                        "rnbqkbnr/pppppppp/8/8/8/5P2/PPPPP1PP/RNBQKBNR b KQkq - 0 1", 
                        "rnbqkbnr/pppppppp/8/8/8/6P1/PPPPPP1P/RNBQKBNR b KQkq - 0 1", 
                        "rnbqkbnr/pppppppp/8/8/8/7P/PPPPPPP1/RNBQKBNR b KQkq - 0 1", 
                        "rnbqkbnr/pppppppp/8/8/P7/8/1PPPPPPP/RNBQKBNR b KQkq a3 0 1", 
                        "rnbqkbnr/pppppppp/8/8/1P6/8/P1PPPPPP/RNBQKBNR b KQkq b3 0 1", 
                        "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq c3 0 1", 
                        "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq d3 0 1", 
                        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1", 
                        "rnbqkbnr/pppppppp/8/8/5P2/8/PPPPP1PP/RNBQKBNR b KQkq f3 0 1", 
                        "rnbqkbnr/pppppppp/8/8/6P1/8/PPPPPP1P/RNBQKBNR b KQkq g3 0 1", 
                        "rnbqkbnr/pppppppp/8/8/7P/8/PPPPPPP1/RNBQKBNR b KQkq h3 0 1",
                        "rnbqkbnr/pppppppp/8/8/8/N7/PPPPPPPP/R1BQKBNR b KQkq - 1 1", 
                        "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR b KQkq - 1 1", 
                        "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1", 
                        "rnbqkbnr/pppppppp/8/8/8/7N/PPPPPPPP/RNBQKB1R b KQkq - 1 1"
                        ]
                    },
                    {   %Specific position
                        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 
                        [
                        "8/2p5/K2p4/1P5r/1R3p1k/8/4P1P1/8 b - - 1 1",
                        "8/2p5/3p4/1P5r/KR3p1k/8/4P1P1/8 b - - 1 1",
                        "8/2p5/3p4/KP5r/R4p1k/8/4P1P1/8 b - - 1 1",
                        "8/2p5/3p4/KP5r/2R2p1k/8/4P1P1/8 b - - 1 1",
                        "8/2p5/3p4/KP5r/3R1p1k/8/4P1P1/8 b - - 1 1",
                        "8/2p5/3p4/KP5r/4Rp1k/8/4P1P1/8 b - - 1 1",
                        "8/2p5/3p4/KP5r/5R1k/8/4P1P1/8 b - - 0 1", % capture
                        "8/2p5/3p4/KP5r/5p1k/1R6/4P1P1/8 b - - 1 1",
                        "8/2p5/3p4/KP5r/5p1k/8/1R2P1P1/8 b - - 1 1",
                        "8/2p5/3p4/KP5r/5p1k/8/4P1P1/1R6 b - - 1 1",
                        "8/2p5/3p4/KP5r/1R3p1k/4P3/6P1/8 b - - 0 1",
                        "8/2p5/3p4/KP5r/1R2Pp1k/8/6P1/8 b - e3 0 1",
                        "8/2p5/3p4/KP5r/1R3p1k/6P1/4P3/8 b - - 0 1",
                        "8/2p5/3p4/KP5r/1R3pPk/8/4P3/8 b - g3 0 1"
                        ]
                    },
                    {   %Specific position
                        "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1", 
                        [
                        "2n5/PPPk4/1n6/8/8/8/4Kppp/5N1N w - - 1 2", 
                        "2n5/PPnk4/8/8/8/8/4Kppp/5N1N w - - 0 2", 
                        "n7/nPPk4/8/8/8/8/4Kppp/5N1N w - - 0 2", 
                        "n7/PPPk4/1n6/8/8/8/4Kppp/5N1N w - - 1 2", 
                        "n7/PPPk4/3n4/8/8/8/4Kppp/5N1N w - - 1 2", 
                        "n7/PPPkn3/8/8/8/8/4Kppp/5N1N w - - 1 2", 
                        "n1n1k3/PPP5/8/8/8/8/4Kppp/5N1N w - - 1 2", 
                        "n1n5/PPk5/8/8/8/8/4Kppp/5N1N w - - 0 2", 
                        "n1n5/PPP1k3/8/8/8/8/4Kppp/5N1N w - - 1 2", 
                        "n1n5/PPP5/2k5/8/8/8/4Kppp/5N1N w - - 1 2", 
                        "n1n5/PPP5/3k4/8/8/8/4Kppp/5N1N w - - 1 2", 
                        "n1n5/PPP5/4k3/8/8/8/4Kppp/5N1N w - - 1 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5r1N w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5n1N w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5b1N w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5q1N w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5NrN w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5NnN w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5NbN w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5NqN w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5N1r w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5N1n w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5N1b w - - 0 2", 
                        "n1n5/PPPk4/8/8/8/8/4Kp1p/5N1q w - - 0 2"
                        ]
                    },
                    {   %Specific position
                        "8/8/8/8/8/8/6k1/4K2R w K - 0 1", 
                        [
                        "8/8/8/8/8/8/6k1/3K3R b - - 1 1",
                        "8/8/8/8/8/8/3K2k1/7R b - - 1 1",
                        "8/8/8/8/8/8/4K1k1/7R b - - 1 1",
                        "8/8/8/8/8/8/6k1/4K1R1 b - - 1 1",
                        "8/8/8/8/8/8/6k1/4KR2 b - - 1 1",
                        "8/8/8/8/8/8/6kR/4K3 b - - 1 1",
                        "8/8/8/8/8/7R/6k1/4K3 b - - 1 1",
                        "8/8/8/8/7R/8/6k1/4K3 b - - 1 1",
                        "8/8/8/7R/8/8/6k1/4K3 b - - 1 1",
                        "8/8/7R/8/8/8/6k1/4K3 b - - 1 1",
                        "8/7R/8/8/8/8/6k1/4K3 b - - 1 1",
                        "7R/8/8/8/8/8/6k1/4K3 b - - 1 1"
                        ]
                    },
                    {   %Specific position
                        "8/1n4N1/2k5/8/8/5K2/1N4n1/8 w - - 0 1", 
                        [
                        "4N3/1n6/2k5/8/8/5K2/1N4n1/8 b - - 1 1",
                        "8/1n6/2k1N3/8/8/5K2/1N4n1/8 b - - 1 1",
                        "8/1n6/2k5/5N2/8/5K2/1N4n1/8 b - - 1 1",
                        "8/1n6/2k5/7N/8/5K2/1N4n1/8 b - - 1 1", 
                        "8/1n4N1/2k5/8/8/5K2/6n1/3N4 b - - 1 1",
                        "8/1n4N1/2k5/8/8/3N1K2/6n1/8 b - - 1 1",
                        "8/1n4N1/2k5/8/2N5/5K2/6n1/8 b - - 1 1",
                        "8/1n4N1/2k5/8/N7/5K2/6n1/8 b - - 1 1", 
                        "8/1n4N1/2k5/8/4K3/8/1N4n1/8 b - - 1 1", 
                        "8/1n4N1/2k5/8/6K1/8/1N4n1/8 b - - 1 1", 
                        "8/1n4N1/2k5/8/8/6K1/1N4n1/8 b - - 1 1", 
                        "8/1n4N1/2k5/8/8/8/1N2K1n1/8 b - - 1 1", 
                        "8/1n4N1/2k5/8/8/8/1N3Kn1/8 b - - 1 1", 
                        "8/1n4N1/2k5/8/8/8/1N4K1/8 b - - 0 1" 
                        ]
                    },
                    {   %Specific position
                        "8/2p5/3p4/KP5r/1R2Pp1k/8/6P1/8 b - e3 0 1", 
                        [
                        "8/8/2pp4/KP5r/1R2Pp1k/8/6P1/8 w - - 0 2", 
                        "8/8/3p4/KPp4r/1R2Pp1k/8/6P1/8 w - c6 0 2", 
                        "8/2p5/8/KP1p3r/1R2Pp1k/8/6P1/8 w - - 0 2", 
                        "8/2p5/3p4/KP5r/1R2P2k/5p2/6P1/8 w - - 0 2", 
                        % "8/2p5/3p4/KP5r/1R5k/4p3/6P1/8 w - - 0 2", % not allowed because in check
                        "8/2p5/3p4/Kr6/1R2Pp1k/8/6P1/8 w - - 0 2", 
                        "8/2p5/3p4/KPr5/1R2Pp1k/8/6P1/8 w - - 1 2", 
                        "8/2p5/3p4/KP1r4/1R2Pp1k/8/6P1/8 w - - 1 2", 
                        "8/2p5/3p4/KP2r3/1R2Pp1k/8/6P1/8 w - - 1 2", 
                        "8/2p5/3p4/KP3r2/1R2Pp1k/8/6P1/8 w - - 1 2", 
                        "8/2p5/3p4/KP4r1/1R2Pp1k/8/6P1/8 w - - 1 2", 
                        "8/2p5/3p3r/KP6/1R2Pp1k/8/6P1/8 w - - 1 2", 
                        "8/2p4r/3p4/KP6/1R2Pp1k/8/6P1/8 w - - 1 2", 
                        "7r/2p5/3p4/KP6/1R2Pp1k/8/6P1/8 w - - 1 2", 
                        "8/2p5/3p4/KP4kr/1R2Pp2/8/6P1/8 w - - 1 2", 
                        "8/2p5/3p4/KP5r/1R2Ppk1/8/6P1/8 w - - 1 2", 
                        "8/2p5/3p4/KP5r/1R2Pp2/6k1/6P1/8 w - - 1 2" 
                        ]
                    },
                    {   %Specific position
                        "r3k3/1K6/8/8/8/8/8/8 w q - 0 1", 
                        [
                        "r3k3/8/2K5/8/8/8/8/8 b q - 1 1",
                        "r3k3/8/1K6/8/8/8/8/8 b q - 1 1",
                        "r3k3/2K5/8/8/8/8/8/8 b q - 1 1",
                        "K3k3/8/8/8/8/8/8/8 b - - 0 1"
                        ]
                    },
                    {   %Castling blocked because king in check
                        % DEBUG
                        "r3k3/8/8/8/Q7/8/8/4K3 b q - 0 1", 
                        [
                        "r4k2/8/8/8/Q7/8/8/4K3 w - - 1 2",
                        "r2k4/8/8/8/Q7/8/8/4K3 w - - 1 2",
                        "r7/5k2/8/8/Q7/8/8/4K3 w - - 1 2",
                        "r7/4k3/8/8/Q7/8/8/4K3 w - - 1 2",
                        "4k3/8/8/8/r7/8/8/4K3 w - - 0 2" 
                        ]
                    },
                    {   %Castling blocked because intermediate position in check
                        "r3k3/8/8/Q7/8/8/8/4K3 b q - 0 1", 
                        [
                        "r4k2/8/8/Q7/8/8/8/4K3 w - - 1 2",
                        "r7/5k2/8/Q7/8/8/8/4K3 w - - 1 2",
                        "r7/4k3/8/Q7/8/8/8/4K3 w - - 1 2",
                        "r7/3k4/8/Q7/8/8/8/4K3 w - - 1 2",
                        "1r2k3/8/8/Q7/8/8/8/4K3 w - - 1 2", 
                        "2r1k3/8/8/Q7/8/8/8/4K3 w - - 1 2", 
                        "3rk3/8/8/Q7/8/8/8/4K3 w - - 1 2", 
                        "4k3/r7/8/Q7/8/8/8/4K3 w - - 1 2", 
                        "4k3/8/r7/Q7/8/8/8/4K3 w - - 1 2", 
                        "4k3/8/8/r7/8/8/8/4K3 w - - 0 2" 
                        ]
                    },
                    {   %Castling blocked because target position in check
                        "r3k3/8/Q7/8/8/8/8/4K3 b q - 0 1", 
                        [
                        "r4k2/8/Q7/8/8/8/8/4K3 w - - 1 2",
                        "r2k4/8/Q7/8/8/8/8/4K3 w - - 1 2",
                        "r7/5k2/Q7/8/8/8/8/4K3 w - - 1 2",
                        "r7/4k3/Q7/8/8/8/8/4K3 w - - 1 2",
                        "r7/3k4/Q7/8/8/8/8/4K3 w - - 1 2",
                        "1r2k3/8/Q7/8/8/8/8/4K3 w - - 1 2", 
                        "2r1k3/8/Q7/8/8/8/8/4K3 w - - 1 2", 
                        "3rk3/8/Q7/8/8/8/8/4K3 w - - 1 2", 
                        "4k3/r7/Q7/8/8/8/8/4K3 w - - 1 2", 
                        "4k3/8/r7/8/8/8/8/4K3 w - - 0 2" 
                        ]
                    },
                    {   %Castling not blocked despite the rook crosses a position in check
                        "r3k3/Q7/8/8/8/8/8/4K3 b q - 0 1", 
                        [
                        "2kr4/Q7/8/8/8/8/8/4K3 w - - 1 2",
                        "r4k2/Q7/8/8/8/8/8/4K3 w - - 1 2",
                        "r2k4/Q7/8/8/8/8/8/4K3 w - - 1 2",
                        "1r2k3/Q7/8/8/8/8/8/4K3 w - - 1 2", 
                        "2r1k3/Q7/8/8/8/8/8/4K3 w - - 1 2", 
                        "3rk3/Q7/8/8/8/8/8/4K3 w - - 1 2", 
                        "4k3/r7/8/8/8/8/8/4K3 w - - 0 2"
                        ]
                    },
                    {   % Specific position
                        "8/2p5/3p4/KP4r1/R4p1k/6P1/4P3/8 b - - 0 1", 
                        [
                        % King moves   
                        "8/2p5/3p4/KP4rk/R4p2/6P1/4P3/8 w - - 1 2", 
                        "8/2p5/3p4/KP4r1/R4pk1/6P1/4P3/8 w - - 1 2", 
                        "8/2p5/3p4/KP4r1/R4p2/6Pk/4P3/8 w - - 1 2", 
                        "8/2p5/3p4/KP4r1/R4p2/6k1/4P3/8 w - - 0 2", 
                        % Rook moves   
                        "8/2p5/3p4/KP6/R4p1k/6r1/4P3/8 w - - 0 2" 
                        ]
                    },
                    {   % Specific position
                        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 0",
                        [
                        % King moves
                        "8/2p5/K2p4/1P5r/1R3p1k/8/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/1P5r/KR3p1k/8/4P1P1/8 b - - 1 0",

                        % Rook moves   
                        "8/2p5/3p4/KP5r/R4p1k/8/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/2R2p1k/8/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/3R1p1k/8/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/4Rp1k/8/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/5R1k/8/4P1P1/8 b - - 0 0",
                        "8/2p5/3p4/KP5r/5p1k/1R6/4P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/5p1k/8/1R2P1P1/8 b - - 1 0",
                        "8/2p5/3p4/KP5r/5p1k/8/4P1P1/1R6 b - - 1 0",
                        
                        %Pawn moves
                        "8/2p5/3p4/KP5r/1R3p1k/4P3/6P1/8 b - - 0 0",
                        "8/2p5/3p4/KP5r/1R2Pp1k/8/6P1/8 b - e3 0 0",
                        "8/2p5/3p4/KP5r/1R3p1k/6P1/4P3/8 b - - 0 0",
                        "8/2p5/3p4/KP5r/1R3pPk/8/4P3/8 b - g3 0 0"
                        ]
                    }
                ],
                all_possible_next_positions_test_acc(TestData);
            _ -> do_nothing       % ?DO_NON_EVOLUTIVE_TESTS
        end
    end
    ].
    
% Evaluate all possible moves and compare to test data
% Moves in test data have the same syntax as the output of roce's divide function
% this means that castling moves are simply expressed by a move of the king
all_possible_moves_test_acc([]) -> true;
all_possible_moves_test_acc(TestData) -> 

    [{Start, ExpectedMoves} | Remaining] = TestData,
    
    ExpectedLength   = length(ExpectedMoves), 
    
    Position         = string_to_position(Start),
    CalculatedMoves  = lists:map(fun(X) -> move_to_string(X) end, chessfold:all_possible_moves(Position)),
    CalculatedLength = length(CalculatedMoves), 
    
    case CalculatedLength - ExpectedLength of
        0 -> true;
        _ -> io:format("~nStart: ~ts", [Start]),
             io:format("~nUnexpected number of results: ~w instead of ~w.", [CalculatedLength, ExpectedLength])
             % show_position_strings(CalculatedPositions)
    end,
    
    SortedExpectedMoves   = lists:sort(ExpectedMoves),
    SortedCalculatedMoves = lists:sort(CalculatedMoves),
    
    if 
        SortedExpectedMoves =:= SortedCalculatedMoves -> true;
        true                                          -> io:format("~nStart: ~ts", [Start]),
                                                         show_list_differences("expected", SortedExpectedMoves, "calculated", SortedCalculatedMoves)
    end,
    
    ?assertEqual(ExpectedLength, CalculatedLength),
    ?assertEqual(SortedExpectedMoves, SortedCalculatedMoves),
    
    all_possible_moves_test_acc(Remaining).
    
        
% Evaluate all possible next positions and compare to test data
all_possible_next_positions_test_acc([]) -> true; 
all_possible_next_positions_test_acc(TestData) -> 
    
    [{Start, ExpectedPositions} | Remaining] = TestData,
    
    ExpectedLength = length(ExpectedPositions), 
    
    Position = string_to_position(Start),
    CalculatedPositions = all_possible_next_position_strings(Position),
    CalculatedLength = length(CalculatedPositions), 
    
    case CalculatedLength - ExpectedLength of
        0 -> true;
        _ -> io:format("~nStart: ~ts", [Start]),
             io:format("~nUnexpected number of results: ~w instead of ~w.", [CalculatedLength, ExpectedLength])
             % show_position_strings(CalculatedPositions)
    end,
    
    SortedExpectedPositions   = lists:sort(ExpectedPositions),
    SortedCalculatedPositions = lists:sort(CalculatedPositions),
    
    if 
        SortedExpectedPositions =:= SortedCalculatedPositions -> true;
        true                                                  -> io:format("~nStart: ~ts", [Start]),
                                                                 show_list_differences("expected", SortedExpectedPositions, "calculated", SortedCalculatedPositions)
    end,
    
    ?assertEqual(ExpectedLength, CalculatedLength),
    ?assertEqual(SortedExpectedPositions, SortedCalculatedPositions),
    
    all_possible_next_positions_test_acc(Remaining).
    
%% -----------------------------------------------------------------------------
% Sources https://chessprogramming.wikispaces.com/Perft+Results
%         http://www.mysnip.de/forum-archiv/thema/1578/296642/Anzahl+Zugfolgen.html
perft1_test_() ->
    {timeout, 1200, [
    fun() ->
        Tests = [
            %  Already in perft_file_test_/0: {
            %  Already in perft_file_test_/0:     "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 
            %  Already in perft_file_test_/0:     [       20,
            %  Already in perft_file_test_/0:            400,
            %  Already in perft_file_test_/0:           8902,
            %  Already in perft_file_test_/0:         197281,
            %  Already in perft_file_test_/0:        4865609,
            %  Already in perft_file_test_/0:      119060324]
            %  Already in perft_file_test_/0: },
            %  Already in perft_file_test_/0: {
            %  Already in perft_file_test_/0:     "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -", 
            %  Already in perft_file_test_/0:     [      48,
            %  Already in perft_file_test_/0:          2039,
            %  Already in perft_file_test_/0:         97862,
            %  Already in perft_file_test_/0:       4085603,
            %  Already in perft_file_test_/0:     193690690]
            %  Already in perft_file_test_/0: },
            {
                "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -", 
                [       14,
                       191,
                      2812,
                     43238,
                    674624,
                  11030083,
                 178633661]
            }
        ],
        perft_acc(Tests, ?PERFT_DEPTH),
        io:format("~nPerft1 test OK.", [])
    end
    ]}.
    
%% -----------------------------------------------------------------------------
% Sources: http://www.albert.nu/programs/sharper/perft.htm 
% and http://www.rocechess.ch/perft.html
perft2_test_() ->
    {timeout, 600, [
    fun() ->
        Tests = [
            %  Already in perft_file_test_/0: {
            %  Already in perft_file_test_/0:     "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1", 
            %  Already in perft_file_test_/0:     [       24,
            %  Already in perft_file_test_/0:            496,
            %  Already in perft_file_test_/0:           9483,
            %  Already in perft_file_test_/0:         182838,
            %  Already in perft_file_test_/0:        3605103,
            %  Already in perft_file_test_/0:       71179139]
            %  Already in perft_file_test_/0: },
            {
                "8/3K4/2p5/p2b2r1/5k2/8/8/1q6 b - - 1 67", 
                [       50,
                       279]
            }
        ],
        perft_acc(Tests, ?PERFT_DEPTH),
        io:format("~nPerft2 test OK.", [])
    end
    ]}.
    
%% -----------------------------------------------------------------------------
% To isolate an error discovered with perft1_test_/0 at depth 5
perft3_test_() ->
    {timeout, 3600, [
    fun() ->
        case ?DO_ISOLATION_TESTS of
            true ->
                Tests = [
                    % Sequels of 8/2p5/8/1P1p3r/RK3p1k/8/4P1P1/8 b - -
                    {
                        "8/8/2p5/1P1p3r/RK3p1k/8/4P1P1/8 w - - 0 1", 
                        [       18]
                    },
                    {
                        "8/8/8/1Ppp3r/RK3p1k/8/4P1P1/8 w - c6 0 1", 
                        [        6]
                    },
                    {
                        "8/2p5/8/1P5r/RK1p1p1k/8/4P1P1/8 w - - 0 1", 
                        [       16]
                    },
                    {
                        "8/2p5/8/1P1p3r/RK5k/5p2/4P1P1/8 w - - 0 1", 
                        [       19]
                    },
                    {
                        "8/2p5/8/1P1p3r/RK3pk1/8/4P1P1/8 w - - 0 1", 
                        [       16]
                    },
                    {
                        "8/2p5/8/1P1p3r/RK3p2/6k1/4P1P1/8 w - - 0 1", 
                        [       15]
                    },
                    {
                        "8/2p5/8/1P1p2kr/RK3p2/8/4P1P1/8 w - - 0 1", 
                        [       17]
                    },
                    {
                        "8/2p5/8/1P1p2r1/RK3p1k/8/4P1P1/8 w - - 0 1", 
                        [       17]
                    },
                    {
                        "8/2p5/8/1P1p1r2/RK3p1k/8/4P1P1/8 w - - 0 1", 
                        [       17]
                    },
                    {
                        "8/2p5/8/1P1pr3/RK3p1k/8/4P1P1/8 w - - 0 1", 
                        [       17]
                    },
                    {
                        "8/2p5/7r/1P1p4/RK3p1k/8/4P1P1/8 w - - 0 1", 
                        [       17]
                    },
                    {
                        "8/2p4r/8/1P1p4/RK3p1k/8/4P1P1/8 w - - 0 1", 
                        [       17]
                    },
                    {
                        "7r/2p5/8/1P1p4/RK3p1k/8/4P1P1/8 w - - 0 1", 
                        [       17]
                    },
                    
                    % Sequels of 8/2p5/8/KP1p3r/R4p1k/8/4P1P1/8 w - -: 
                    {
                        "8/2p5/K7/1P1p3r/R4p1k/8/4P1P1/8 b - - 0 1", 
                        [       12,
                               190]
                    },
                    {
                        "8/2p5/8/1P1p3r/RK3p1k/8/4P1P1/8 b - - 0 1", 
                        [       13,
                               209]
                    },
                    {
                        "8/2p5/1P6/K2p3r/R4p1k/8/4P1P1/8 b - - 0 1", 
                        [       13,
                               186]
                    },
                    {
                        "8/2p5/8/KP1p3r/R4p1k/4P3/6P1/8 b - - 0 1", 
                        [       12,
                               177]
                    },
                    {
                        "8/2p5/8/KP1p3r/R3Pp1k/8/6P1/8 b - - 0 1", 
                        [       14,
                               178]
                    },
                    {
                        "8/2p5/8/KP1p3r/R4p1k/6P1/4P3/8 b - - 0 1", 
                        [        4,
                                57]
                    },
                    {
                        "8/2p5/8/KP1p3r/R4pPk/8/4P3/8 b - - 0 1", 
                        [       14,
                               201]
                    },
                    {
                        "8/2p5/8/KP1p3r/1R3p1k/8/4P1P1/8 b - - 0 1", 
                        [       12,
                               177]
                    },
                    {
                        "8/2p5/8/KP1p3r/2R2p1k/8/4P1P1/8 b - - 0 1", 
                        [       13,
                               228]
                    },
                    {
                        "8/2p5/8/KP1p3r/3R1p1k/8/4P1P1/8 b - - 0 1", 
                        [       11,
                               186]
                    },
                    {
                        "8/2p5/8/KP1p3r/4Rp1k/8/4P1P1/8 b - - 0 1", 
                        [       13,
                               202]
                    },
                    {
                        "8/2p5/8/KP1p3r/5R1k/8/4P1P1/8 b - - 0 1", 
                        [        2,
                                42]
                    },
                    {
                        "8/2p5/8/KP1p3r/5p1k/R7/4P1P1/8 b - - 0 1", 
                        [       12,
                               216]
                    },
                    {
                        "8/2p5/8/KP1p3r/5p1k/8/R3P1P1/8 b - - 0 1", 
                        [       13,
                               182]
                    },
                    {
                        "8/2p5/8/KP1p3r/5p1k/8/4P1P1/R7 b - - 0 1", 
                        [       13,
                               234]
                    },
                    
                    % Sequels of 8/2p5/3p4/KP5r/R4p1k/8/4P1P1/8 b - -: 
                    {
                        "8/8/2pp4/KP5r/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [       15,
                               207,
                              3341]
                    },
                    {
                        "8/8/3p4/KPp4r/R4p1k/8/4P1P1/8 w - c6 0 1", 
                        [       15,
                               168,
                              2673]
                    },
                    {
                        "8/2p5/8/KP1p3r/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [       15,
                               171,
                              2665]
                    },
                    {
                        "8/2p5/3p4/KP5r/R4pk1/8/4P1P1/8 w - - 0 1", 
                        [       13,
                               246,
                              3570]
                    },
                    {
                        "8/2p5/3p4/KP4kr/R4p2/8/4P1P1/8 w - - 0 1", 
                        [       15,
                               253,
                              4060]
                    },
                    {
                        "8/2p5/3p4/KP5r/R4p2/6k1/4P1P1/8 w - - 0 1", 
                        [       12,
                               247,
                              3494]
                    },
                    {
                        "8/2p5/3p4/KP4r1/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [       14,
                               227,
                              3331]
                    },
                    {
                        "8/2p5/3p4/KP3r2/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [       14,
                               206,
                              3037]
                    },
                    {
                        "8/2p5/3p4/KP2r3/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [       14,
                               236,
                              3416]
                    },
                    {
                        "8/2p5/3p4/KP1r4/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [       14,
                               202,
                              2924]
                    },
                    {
                        "8/2p5/3p4/KPr5/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [       14,
                               214,
                              3056]
                    },
                    {
                        "8/2p5/3p4/Kr6/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [        2,
                                28,
                               411]
                    },
                    {
                        "8/2p5/3p3r/KP6/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [       15,
                               183,
                              2899]
                    },
                    {
                        "8/2p4r/3p4/KP6/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [       15,
                               196,
                              3130]
                    },
                    {
                        "7r/2p5/3p4/KP6/R4p1k/8/4P1P1/8 w - - 0 1", 
                        [       15,
                               235,
                              3584]
                    },
                    
                    % Sequels of 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -: 
                    {
                        "8/2p5/3p4/KP5r/1R3p1k/6P1/4P3/8 b - - 0 1", 
                        [        4,
                                54,
                              1014,
                             14747]
                    },
                    {
                        "8/2p5/3p4/KP5r/1R3pPk/8/4P3/8 b - g3 0 1", 
                        [       17,
                               226,
                              3702,
                             53895]
                    },
                    {
                        "8/2p5/3p4/KP5r/1R3p1k/4P3/6P1/8 b - - 0 1", 
                        [       15,
                               205,
                              3107,
                             45326]
                    },
                    {
                        "8/2p5/3p4/KP5r/1R2Pp1k/8/6P1/8 b - - 0 1", 
                        [       16,
                               177,
                              2748,
                             36889]
                    },
                    {
                        "8/2p5/K2p4/1P5r/1R3p1k/8/4P1P1/8 b - - 0 1", 
                        [       15,
                               240,
                              3653,
                             59028]
                    },
                    {
                        "8/2p5/3p4/1P5r/KR3p1k/8/4P1P1/8 b - - 0 1", 
                        [       15,
                               224,
                              3394,
                             52943]
                    },
                    {
                        "8/2p5/3p4/KP5r/R4p1k/8/4P1P1/8 b - - 0 1", 
                        [       15,
                               202,
                              3019,
                             45591]
                    },
                    {
                        "8/2p5/3p4/KP5r/2R2p1k/8/4P1P1/8 b - - 0 1", 
                        [       15,
                               254,
                              3797,
                             63781]
                    },
                    {
                        "8/2p5/3p4/KP5r/3R1p1k/8/4P1P1/8 b - - 0 1", 
                        [       15,
                               243,
                              3622,
                             59574]
                    },
                    {
                        "8/2p5/3p4/KP5r/4Rp1k/8/4P1P1/8 b - - 0 1", 
                        [       15,
                               228,
                              3391,
                             54192]
                    },
                    {
                        "8/2p5/3p4/KP5r/5R1k/8/4P1P1/8 b - - 0 1", 
                        [        2,
                                41,
                               606,
                             10776]
                    },
                    {
                        "8/2p5/3p4/KP5r/5p1k/1R6/4P1P1/8 b - - 0 1", 
                        [       15,
                               248,
                              3658,
                             59719]
                    },
                    {
                        "8/2p5/3p4/KP5r/5p1k/8/1R2P1P1/8 b - - 0 1", 
                        [       16,
                               205,
                              3328,
                             48498]
                    },
                    {
                        "8/2p5/3p4/KP5r/5p1k/8/4P1P1/1R6 b - - 0 1", 
                        [       16,
                               265,
                              4199,
                             69665]
                    }
                ],
                perft_acc(Tests, ?PERFT_DEPTH),
                io:format("~nPerft3 test OK.", []);
            _ -> do_nothing
        end
    end
    ]}.
    
%% -----------------------------------------------------------------------------
% Source: http://mediocrechess.blogspot.com/2007/01/guide-perft-scores.html
perft_file_test_() ->
    {timeout, 604800, [
    fun() ->
        Lines = read_file_lines(?PERFT_SUITE),
        
        Tests = perft_lines_to_tests(Lines, []),
        
        RevTest = lists:reverse(Tests),
        
        ?debugTime(io_lib:format("Overall duration with depth ~b", [?PERFT_DEPTH]), perft_acc(RevTest, ?PERFT_DEPTH))
    end
    ]}.
    
perft_lines_to_tests(   [], Accum) -> Accum;
perft_lines_to_tests(Lines, Accum) -> 
    [Line | RemainingLines] = Lines,
    case lists:nth(1, Line) of
        $% -> 
            ?debugMsg(io_lib:format("Line skipped: ~s", [Line])),
            perft_lines_to_tests(RemainingLines, Accum); % Line skipped because commented out
        _ ->
            [Start | IterationStrings] = string:tokens(Line, ";"),
            IterationValues = perft_iteration_strings_to_values(IterationStrings, []),
            Test = {Start, lists:reverse(IterationValues)}, 
            perft_lines_to_tests(RemainingLines, [Test | Accum])
    end.
    
perft_iteration_strings_to_values(              [], Accum) -> Accum;
perft_iteration_strings_to_values(IterationStrings, Accum) ->
    [IterationString | Remainings] = IterationStrings,
    [_, IterationValue] = string:tokens(string:strip(IterationString), " "),
    IterationValueNoSpace = string:strip(IterationValue), 
    IterationValueNoLineFeed = string:strip(IterationValueNoSpace, both, 10), % Decimal code of line feed 
    IntValue = list_to_integer(string:strip(IterationValueNoLineFeed)),
    perft_iteration_strings_to_values(Remainings, [IntValue | Accum]).
    
%% -----------------------------------------------------------------------------
%% Divide and conquer test
%% To compare with roce divide function
division_test_() ->
    {timeout, 1800, [
    fun() ->
        case ?DO_NON_EVOLUTIVE_TESTS of
            true ->
                % To compare to roce divide N, with ExpectedNumber = 1 if N = 1, individual totals depth 2 if N = 2, etc. 
                % Format: {Start, DepthAfterDivide, [{MoveString, ExpectedNumber}, ...]} 
                TestData = [
                    {
                        "1r2k2r/8/8/8/8/8/8/R3K2R w KQk - 0 1", 
                        2, 
                        [
                        {"e1g1", 22 },
                        {"e1c1", 23 },
                        {"e1f1", 25 },
                        {"e1d1", 25 },
                        {"e1e2", 25 },
                        {"e1f2", 25 },
                        {"e1d2", 25 },
                        {"a1b1", 25 },
                        {"a1c1", 25 },
                        {"a1d1", 23 },
                        {"a1a2", 25 },
                        {"a1a3", 25 },
                        {"a1a4", 25 },
                        {"a1a5", 25 },
                        {"a1a6", 25 },
                        {"a1a7", 22 },
                        {"a1a8", 18 },
                        {"h1g1", 24 },
                        {"h1f1", 22 },
                        {"h1h2", 24 },
                        {"h1h3", 23 },
                        {"h1h4", 22 },
                        {"h1h5", 21 },
                        {"h1h6", 20 },
                        {"h1h7", 16 },
                        {"h1h8", 3  }
                        ]
                    },
                    {
                        "8/2p5/3p4/KP4r1/R4p1k/8/4P1P1/8 w - - 0 1", 
                        2, 
                        [
                        {"a5a6", 18 },
                        {"a5b4", 19 },
                        {"e2e3", 18 },
                        {"e2e4", 19 },
                        {"g2g3", 5  },
                        {"g2g4", 17 },
                        {"a4b4", 18 },
                        {"a4c4", 18 },
                        {"a4d4", 18 },
                        {"a4e4", 18 },
                        {"a4f4", 3  },
                        {"a4a3", 18 },
                        {"a4a2", 19 },
                        {"a4a1", 19 }
                        ]
                    },
                    {
                        "4k3/8/8/8/8/8/8/4K2R w K - 0 1", 
                        2, 
                        [
                        {"e1g1", 3}, 
                        {"e1f1", 5}, 
                        {"e1d1", 5}, 
                        {"e1e2", 5}, 
                        {"e1f2", 5}, 
                        {"e1d2", 5}, 
                        {"h1g1", 5}, 
                        {"h1f1", 3}, 
                        {"h1h2", 5}, 
                        {"h1h3", 5}, 
                        {"h1h4", 5}, 
                        {"h1h5", 5}, 
                        {"h1h6", 5}, 
                        {"h1h7", 2}, 
                        {"h1h8", 3}
                        ]
                    },
                    {
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 
                        2, 
                        [
                        {"e1g1", 43},
                        {"e1c1", 43},
                        {"e1f1", 43},
                        {"e1d1", 43},
                        {"d5d6", 41},
                        {"d5e6", 46},
                        {"a2a3", 44},
                        {"a2a4", 44},
                        {"b2b3", 42},
                        {"g2g3", 42},
                        {"g2g4", 42},
                        {"g2h3", 43},
                        {"e5d3", 43},
                        {"e5f7", 44},
                        {"e5c4", 42},
                        {"e5g6", 42},
                        {"e5g4", 44},
                        {"e5c6", 41},
                        {"e5d7", 45},
                        {"c3b1", 42},
                        {"c3a4", 42},
                        {"c3d1", 42},
                        {"c3b5", 39},
                        {"f3g3", 43},
                        {"f3h3", 43},
                        {"f3e3", 43},
                        {"f3d3", 42},
                        {"f3g4", 43},
                        {"f3h5", 43},
                        {"f3f4", 43},
                        {"f3f5", 45},
                        {"f3f6", 39},
                        {"d2c1", 43},
                        {"d2e3", 43},
                        {"d2f4", 43},
                        {"d2g5", 42},
                        {"d2h6", 41},
                        {"e2d1", 44},
                        {"e2f1", 44},
                        {"e2d3", 42},
                        {"e2c4", 41},
                        {"e2b5", 39},
                        {"e2a6", 36},
                        {"a1b1", 43},
                        {"a1c1", 43},
                        {"a1d1", 43},
                        {"h1g1", 43},
                        {"h1f1", 43}
                        ]
                    }
                  , {
                        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 
                        3, 
                        [
                        {"e1g1", 2059},
                        {"e1c1", 1887}, 
                        {"e1f1", 1855},
                        {"e1d1", 1894},
                        {"d5d6", 1991},
                        {"d5e6", 2241},
                        {"a2a3", 2186},
                        {"a2a4", 2149},
                        {"b2b3", 1964},
                        {"g2g3", 1882},
                        {"g2g4", 1843},
                        {"g2h3", 1970},
                        {"e5d3", 1803},
                        {"e5f7", 2080},
                        {"e5c4", 1880},
                        {"e5g6", 1997},
                        {"e5g4", 1878},
                        {"e5c6", 2027},
                        {"e5d7", 2124},
                        {"c3b1", 2038},
                        {"c3a4", 2203},
                        {"c3d1", 2040},
                        {"c3b5", 2138},
                        {"f3g3", 2214},
                        {"f3h3", 2360},
                        {"f3e3", 2174},
                        {"f3d3", 2005},
                        {"f3g4", 2169},
                        {"f3h5", 2267},
                        {"f3f4", 2132},
                        {"f3f5", 2396},
                        {"f3f6", 2111},
                        {"d2c1", 1963},
                        {"d2e3", 2136},
                        {"d2f4", 2000},
                        {"d2g5", 2134},
                        {"d2h6", 2019},
                        {"e2d1", 1733},
                        {"e2f1", 2060},
                        {"e2d3", 2050},
                        {"e2c4", 2082},
                        {"e2b5", 2057},
                        {"e2a6", 1907},
                        {"a1b1", 1969},
                        {"a1c1", 1968},
                        {"a1d1", 1885},
                        {"h1g1", 2013},
                        {"h1f1", 1929}
                        ]
                    }
                ],
            
                divide_and_conquer_array(TestData);
        _ -> do_nothing  % Not ?DO_NON_EVOLUTIVE_TESTS
        end
    end
    ]}.
    
divide_and_conquer_array([]) -> do_nothing;
divide_and_conquer_array(TestData) -> 
    [{Start, Depth, ExpectedResults} | RemainingTestData] = TestData,
    StartPosition = string_to_position(Start), 
    PossibleMoves = chessfold:all_possible_moves(StartPosition),
    
    divide_and_conquer(StartPosition, Depth - 1, PossibleMoves, ExpectedResults),
    
    divide_and_conquer_array(RemainingTestData).

divide_and_conquer(_, _, [], []) -> do_nothing;

divide_and_conquer(_, _, PossibleMoves, []) -> 
    [PossibleMove | RemainingPossibleMoves] = PossibleMoves,
    io:format("~nMissing result: ~w", [move_to_string(PossibleMove)]),
    divide_and_conquer(false, false, RemainingPossibleMoves, []);
    
divide_and_conquer(_, _, [], ExpectedResults) -> 
    [{ExpectedMoveString, _} | RemainingExpectedResults] = ExpectedResults,
    io:format("~nMissed move string: ~ts", [ExpectedMoveString]),
    divide_and_conquer(false, false, [], RemainingExpectedResults);
    
divide_and_conquer(StartPosition, DepthAfterDivide, PossibleMoves, ExpectedResults) ->
    [{ExpectedMoveString, ExpectedResult} | RemainingExpectedResults] = ExpectedResults,
    
    ExpectedMove = string_to_move(StartPosition, ExpectedMoveString),
    ?assert(false =/= ExpectedMove),
    RemainingPossibleMoves = lists:delete(ExpectedMove, PossibleMoves),
    ExpectedEndPositionString = position_to_string(chessfold:position_after_move(ExpectedMove)),
    
    TestArray = case DepthAfterDivide of
        1 -> [                                                 ExpectedResult];
        2 -> [                                          false, ExpectedResult];
        3 -> [                                   false, false, ExpectedResult];
        4 -> [                            false, false, false, ExpectedResult];
        5 -> [                     false, false, false, false, ExpectedResult];
        6 -> [              false, false, false, false, false, ExpectedResult];
        7 -> [       false, false, false, false, false, false, ExpectedResult];
        8 -> [false, false, false, false, false, false, false, ExpectedResult];
        _ -> throw({invalid_test_depth, DepthAfterDivide})
    end,
    
    Tests = [
        {
            ExpectedEndPositionString, 
            TestArray
        }
    ],
    
    perft_acc(Tests, DepthAfterDivide),
    
    % Continue with the next result
    divide_and_conquer(StartPosition, DepthAfterDivide, RemainingPossibleMoves, RemainingExpectedResults).
    
%% -----------------------------------------------------------------------------
% Common to all perft tests
perft_acc([], _) -> true;
perft_acc(Tests, Depth) ->
    [Test | RemainingTests] = Tests,
    perft_execute(Test, Depth), 
    perft_acc(RemainingTests, Depth).
    
perft_execute(   _, Depth) when Depth =< 0 -> throw({invalid_depth, Depth});
perft_execute(Test, Depth) -> 
    
    {Start, Stats} = Test,
    
    ?debugMsg(io_lib:format("Position: ~ts", [Start])),
    
    ?debugTime("Duration", perft_test_position(Start, Stats, Depth)).
    
perft_test_position(Start, Stats, Depth) ->    
    % Total dictionary with initial values = 0
    InitialTotalDict = total_dict_fill(1, Depth, orddict:new()),
    
    StatLength = length(Stats),
    ActualDepth = if
        Depth =< StatLength -> Depth;
        true                -> StatLength
    end,
    
    TotalDict = perft_explore_position(string_to_position(Start), 1, ActualDepth, InitialTotalDict),
    verify_stats(Start, Stats, 1, ActualDepth, TotalDict).
    
total_dict_fill(Level, Depth, TotalDict) when Depth < Level -> TotalDict;
total_dict_fill(Level, Depth, TotalDict) ->
    total_dict_fill(Level + 1, Depth, orddict:store(Level, 0, TotalDict)).
    
perft_explore_position(       _, Level, Depth, TotalDict) when Depth < Level -> TotalDict;
perft_explore_position(Position, Level, Depth, TotalDict) ->

    NextPositions = all_possible_next_position_records(Position),
    
    % Add number of positions to dictionary
    NewTotalDict = orddict:update(Level, fun(X) -> X + length(NextPositions) end, TotalDict),
    
    % Returns a new total dictionary: 
    perft_explore_level(Level, NextPositions, Depth, NewTotalDict).
    
perft_explore_level(    _,        [],     _, TotalDict) -> TotalDict;
perft_explore_level(Level, Positions, Depth, TotalDict) ->
    [Position | Remainings] = Positions, 
    
    TotalDict1 = perft_explore_position(Position, Level + 1, Depth, TotalDict),
    
    % Returns a new total dictionary: 
    perft_explore_level(Level, Remainings, Depth, TotalDict1). 
    
verify_stats(    _,     _, Level, Depth,         _) when Depth < Level -> do_noting;
verify_stats(Start, Stats, Level, Depth, TotalDict) ->
    [ExpectedTotal | RemainingStats] = Stats,
    
    CalculatedTotal = orddict:fetch(Level, TotalDict),

    case ExpectedTotal of
        false -> do_noting;
        _     -> case ExpectedTotal - CalculatedTotal of
                     0 -> true;
                     _ -> io:format("~nStart: ~ts", [Start]),
                          io:format("~nUnexpected number of results: ~w instead of ~w.", [CalculatedTotal, ExpectedTotal])
                 end,
                 
                 ?assertEqual(ExpectedTotal, CalculatedTotal)
    end,

    verify_stats(Start, RemainingStats, Level + 1, Depth, TotalDict).

%% -----------------------------------------------------------------------------
%% @doc Transform a string into a move Tuple
%% @spec string_to_move(tuple(), string()) -> tuple() or false if the move is invalid
string_to_move(Position, String) ->
    
    % Verify move string format
    % Using length here is OK since the list contains 4 elements (http://www.erlang.org/doc/efficiency_guide/commoncaveats.html)
    if 
        length(String) =:= 4 -> do_nothing;
        true                 -> throw({invalid_move_string, String})
    end,
    
    [FromColChar, FromRowChar, ToColChar, ToRowChar] = String,
    
    FromColId = FromColChar - $a,
    FromRowId = FromRowChar - $1,
    ToColId   = ToColChar - $a,
    ToRowId   = ToRowChar - $1,
    
    From = chessfold:square_reference(FromRowId, FromColId),
    To   = chessfold:square_reference(  ToRowId,   ToColId),
    
    PossibleMoves = chessfold:all_possible_moves_from(Position, From),
    
    extract_move_with_destination(PossibleMoves, To).
    
    extract_move_with_destination([], _) -> false;    
    extract_move_with_destination(PossibleMoves, To) ->
        [Move | RemainingMoves] = PossibleMoves, 
        
        case chessfold:piece_square(chessfold:move_target(Move)) of
            To -> Move;
            _  -> extract_move_with_destination(RemainingMoves, To)
        end.

%% -----------------------------------------------------------------------------
%% @doc Transform a move tuple into a string. This string can be used by javascript code. 
%% @spec move_to_string(tuple()) -> string()
move_to_string(Move) ->
    square_to_string(chessfold:move_origin(Move)) ++ square_to_string(chessfold:move_target(Move)).
    
%% -----------------------------------------------------------------------------
%% @doc Get all possible next positions of a specific position. 
%% Each element of the resulting list contains a Forsyth-Edwards string. 
%% It is important that the input strings and output strings be exempt from any 
%% clock or counter information, which cannot be taken into account in Gigambit. 
%% @spec all_possible_next_position_records(term()) -> list()
all_possible_next_position_records(Position)  ->
    Moves = chessfold:all_possible_moves(Position),
    MoveToPosition = fun(Move) -> 
        chessfold:position_after_move(Move)
    end,
    lists:map(MoveToPosition, Moves).

all_possible_next_position_strings(Position) ->
    Moves = chessfold:all_possible_moves(Position),
    MoveToPosition = fun(Move) -> 
        NewPosition = chessfold:position_after_move(Move),
        chessfold:position_to_string(NewPosition)
    end,
    lists:map(MoveToPosition, Moves).
    
%% -----------------------------------------------------------------------------
%% Source: http://www.trapexit.org/Read_File_to_List
read_file_lines(FileName) ->
    case file:open(FileName, [read]) of
        {ok, Device} -> Result = get_all_lines(Device, []),
                        file:close(Device), 
                        Result;
        Error        -> Error
    end.

    get_all_lines(Device, Accum) ->
        case io:get_line(Device, "") of
            eof  -> lists:reverse(Accum);
            Line -> get_all_lines(Device, [Line|Accum])
        end.

%% -----------------------------------------------------------------------------
show_list_differences(         _,    [],          _,     []) -> true;

show_list_differences(         _, List1, Adjective2,     []) ->
    [Element1 | Remaining1] = List1,
    io:format("~nElement ~ts was not ~ts", [Element1, Adjective2]), 
    show_list_differences(false, Remaining1, Adjective2, []);

show_list_differences(Adjective1,    [],          _, List2) ->
    [Element2 | Remaining2] = List2,
    io:format("~nElement ~ts was not ~ts", [Element2, Adjective1]), 
    show_list_differences(Adjective1, [], false, Remaining2);

show_list_differences(Adjective1, List1, Adjective2, List2) ->
    [Element1 | Remaining1] = List1,
    [Element2 | Remaining2] = List2,
    
    if
        Element1 < Element2 -> io:format("~nElement ~ts was not ~ts", [Element1, Adjective2]), 
                               show_list_differences(Adjective1, Remaining1, Adjective2, List2);
        Element1 > Element2 -> io:format("~nElement ~ts was not ~ts", [Element2, Adjective1]),
                               show_list_differences(Adjective1, List1,      Adjective2, Remaining2); 
        true                -> show_list_differences(Adjective1, Remaining1, Adjective2, Remaining2)
    end.
   

