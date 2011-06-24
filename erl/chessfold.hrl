%% @doc Chess Move Generator: Header file
%% @end
%% 
%% Author François Cardinaux, CH 1207 Genève
%% Copyright 2011 François Cardinaux

-record(ntc_chess_position, {
            pieces,                 % a list of ntc_chess_piece records
            turn, 
            allowedCastling         = 0, 
            enPassantSquare         = false, 
            halfMoveClock           = 0, 
            moveNumber              = 0}).
            
-record(ntc_chess_piece, {
            color, 
            type, 
            square = false}).
            
-record(ntc_chess_move, {
            from,               % ntc_chess_piece (i.e. with square)
            to,                 % ntc_chess_piece (i.e. with square)
            newPosition,        % ntc_chess_position
            castling            = false,    % false, king or queen
            taken               = false}).  % ntc_chess_piece (i.e. with square of taken piece, which may be different from 'to')

