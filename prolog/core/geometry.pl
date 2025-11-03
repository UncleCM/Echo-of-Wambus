% =============================================================================
% MODULE: core/geometry.pl
% PURPOSE: Basic geometric calculations and collision detection
% =============================================================================

:- module(geometry, [
    point_in_rect/6,
    rects_collide/8,
    distance_squared/5,
    normalize_direction/6,
    normalize_vector/4
]).

% =============================================================================
% COLLISION DETECTION
% =============================================================================

% Check if a point is inside a rectangle
% point_in_rect(X, Y, RectX, RectY, RectW, RectH)
point_in_rect(X, Y, RectX, RectY, RectW, RectH) :-
    X >= RectX,
    X =< RectX + RectW,
    Y >= RectY,
    Y =< RectY + RectH.

% Check if two rectangles collide (AABB collision detection)
% rects_collide(X1, Y1, W1, H1, X2, Y2, W2, H2)
rects_collide(X1, Y1, W1, H1, X2, Y2, W2, H2) :-
    X1 < X2 + W2,
    X1 + W1 > X2,
    Y1 < Y2 + H2,
    Y1 + H1 > Y2.

% =============================================================================
% DISTANCE CALCULATIONS
% =============================================================================

% Calculate distance squared between two points (avoids expensive sqrt)
% distance_squared(X1, Y1, X2, Y2, DistSq)
distance_squared(X1, Y1, X2, Y2, DistSq) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    DistSq is DX * DX + DY * DY.

% =============================================================================
% VECTOR OPERATIONS
% =============================================================================

% Normalize a vector (DX, DY) to unit length
% normalize_vector(DX, DY, NormX, NormY)
normalize_vector(DX, DY, NormX, NormY) :-
    Magnitude is sqrt(DX * DX + DY * DY),
    (Magnitude > 0 ->
        NormX is DX / Magnitude,
        NormY is DY / Magnitude
    ;
        NormX = 0.0,
        NormY = 0.0
    ).

% Calculate normalized direction vector from point A to point B
% Returns direction as floats between -1.0 and 1.0
% normalize_direction(X1, Y1, X2, Y2, DirX, DirY)
normalize_direction(X1, Y1, X2, Y2, DirX, DirY) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    normalize_vector(DX, DY, DirX, DirY).
