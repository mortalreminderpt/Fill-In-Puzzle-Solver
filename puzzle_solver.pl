%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author:   mortalreminder <mortalreminder.pt@outlook.com>
% Purpose:  Program provides a predicate puzzle_solution(+Puzzle, +WordList)
%           for solving a fill-in puzzle. It holds when Puzzle is a
%           representation of a solved puzzle corresponding to a given list
%           of words, WordList.
% Approach:
%           1. Extract consecutive fillable elements (not '#') in rows and
%              columns as slots, ignoring slots with length <= 1.
%           2. Matching slot: select the word for the slot in turn, and remove
%              the word and slot from the list to be matched if unification
%              succeeds.
%           3. Count the number of matching words for each slot; the slot with
%              the fewest matches can reduce backtracking and improve
%              efficiency.
%           4. The solution succeeds if all slots are successfully matched.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(clpfd)).

%% puzzle_solution(+Puzzle:list, +WordList:list) is semidet
%
%  Solves a fill-in puzzle, filling Puzzle with words from WordList.
%   - Puzzle: the puzzle to be solved.
%   - WordList: list of words to be filled into the puzzle.
%
%  Example:
%    ?- puzzle_solution([[#, _, #], [_, _, _], [#, _, #]],
%                        [[h, a, t], [b, a, g]]).
%    true.
puzzle_solution(Puzzle, WordList) :-
    extract_slot_list(Puzzle, SlotList),
    match_slot_list(SlotList, WordList).

%% extract_slot_list(+Puzzle:list, -SlotList:list) is det.
%
%  Extract and filter all slots from a puzzle.
%   - Puzzle: the puzzle (list of rows) to be processed.
%   - SlotList: final list of cell sublists (each representing a slot).
%
%  Example:
%    ?- extract_slot_list([[#, _, #], [_, _, _], [#, _, #]], SlotList).
%    SlotList = [[_, _A, _], [_, _A, _]].
extract_slot_list(Puzzle, SlotList) :-
    extract_puzzle_slot_list(Puzzle, RowSlotList),
    transpose(Puzzle, PuzzleT),
    extract_puzzle_slot_list(PuzzleT, ColSlotList),
    append(RowSlotList, ColSlotList, AllSlotList),
    filter_slot(AllSlotList, SlotList).

%% extract_puzzle_slot_list(+Puzzle:list, -SlotList:list) is det.
%
%  Extract all row slots from a puzzle.
%   - Puzzle: the list of rows representing the puzzle.
%   - SlotList: final list of cell sublists extracted from each row.
%
%  Example:
%    ?- extract_puzzle_slot_list([[#, _, #], [_, _, _], [#, _, #]], SlotList).
%    SlotList = [[_], [_, _, _], [_]].
extract_puzzle_slot_list([], []).
extract_puzzle_slot_list([Row|RestRowList], SlotList) :-
    split_row_slot_list(Row, [], RowSlotList),
    extract_puzzle_slot_list(RestRowList, RestSlotList),
    append(RowSlotList, RestSlotList, SlotList).

%% split_row_slot_list(+Row:list, +Acc:list, -SlotList:list) is det.
%
%  Split a row (list of cells) into sublists (slots).
%   - Row: original list of cells.
%   - Acc: accumulator used to store a temporary slot between '#' delimiters.
%   - SlotList: final list of cell sublists representing a slot.
%
%  Example:
%    ?- split_row_slot_list([a, '#', b, c, d, e, '#', '#', _], [], SlotList).
%    SlotList = [[a], [b, c, d, e], [_]].
split_row_slot_list([], [], []).
split_row_slot_list([], Acc, [Acc]) :-
    Acc \= [].
split_row_slot_list([Cell|RestCellList], Acc, [Acc|SlotList]) :-
    Cell == '#',
    Acc \= [],
    % Only add slot when accumulator is not empty
    split_row_slot_list(RestCellList, [], SlotList).
split_row_slot_list([Cell|RestCellList], [], SlotList) :-
    Cell == '#',
    % When found '#' but accumulator is empty, means consecutive delimiters or
    % delimiter is at the start. skip directly to avoid empty slot generated.
    split_row_slot_list(RestCellList, [], SlotList).
split_row_slot_list([Cell|RestCellList], Acc, SlotList) :-
    Cell \== '#',
    append(Acc, [Cell], NewAcc),
    split_row_slot_list(RestCellList, NewAcc, SlotList).

%% filter_slot(+SlotList:list, -FilteredSlotList:list) is det.
%
%  Filter slots, keeping only those with more than 1 element.
%   - SlotList: list of slots before filtering.
%   - FilteredSlotList: list after removing slots with length <= 1.
%
%  Example:
%    ?- filter_slot([[a], [b, c], [], [d, e, _]], FilteredSlotList).
%    FilteredSlotList = [[b, c], [d, e, _]].
filter_slot([], []).
filter_slot([Slot|RestSlotList], [Slot|FilteredSlotList]) :-
    length(Slot, L),
    L > 1,
    filter_slot(RestSlotList, FilteredSlotList).
filter_slot([Slot|RestSlotList], FilteredSlotList) :-
    length(Slot, L),
    L =< 1,
    filter_slot(RestSlotList, FilteredSlotList).

%% match_slot_list(+SlotList:list, +WordList:list) is semidet.
%
%  Matches each slot in SlotList with a word from WordList.
%  The slot with the fewest matching words is chosen first for
%  pruning the search space.
%   - SlotList: list of slots to be matched.
%   - WordList: list of available words.
%
%  Match Process:
%   1. Succeed when both SlotList and WordList are empty.
%   2. Find the slot with the minimum match count (MinSlot).
%   3. Try to match a word from WordList to MinSlot.
%   4. Remove the matched slot and word, then recursively match the rest.
%
%  Example:
%    ?- match_slot_list([[_, A, _], [_, A, _]], [[h, a, t], [b, a, g]]).
%    A = a.
match_slot_list([], []).
match_slot_list(SlotList, WordList) :-
    find_min_slot(SlotList, WordList, MinSlot),
    member(Word, WordList),
    MinSlot = Word,
    % use remove_first/3 to remove min slot and corresponding word, faster than
    % select/3
    remove_first(SlotList, MinSlot, RestSlotList),
    remove_first(WordList, Word, RestWordList),
    match_slot_list(RestSlotList, RestWordList),
    % We dont need to list all solutions, we just need to find any.
    % Prune to ensure only one solution is found.
    !.

%% find_min_slot(+SlotList:list, +WordList:list, -MinSlot:term) is det.
%
%  Finds the slot from SlotList that has the fewest matching words in
%  WordList.
%   - SlotList: list of available slots.
%   - WordList: list of available words.
%   - MinSlot: the slot with the minimum number of matches.
%
%  Example:
%    ?- find_min_slot([[_,_,_], [a,_,c], [_,b,_]],
%                      [[a,b,c], [a,c,c]], MinSlot).
%    MinSlot = [_, b, _].
find_min_slot([Slot|RestSlotList], WordList, MinSlot) :-
    count_matches(Slot, WordList, FirstCount),
    find_min_slot_helper(RestSlotList, WordList, Slot, FirstCount, MinSlot).

%% find_min_slot_helper(+SlotList:list, +WordList:list, +CurrentMin:term,
%                        +MinCount:int, -MinSlot:term) is det.
%
%  Helper predicate for find_min_slot/3 to iterate over the list of slots.
%   - SlotList: remaining slots to check
%   - WordList: list of available words to match against
%   - CurrentMin: the slot with minimum matches found so far
%   - MinCount: the number of matches for CurrentMin
%   - MinSlot: final slot with minimum matches (output)
%
%  Example:
%    ?- find_min_slot_helper([[a,_,c], [_,b,_]], [[a,b,c], [a,c,c]],
%                            [_,_,_], 3, MinSlot).
%    MinSlot = [_, b, _].
find_min_slot_helper([], _, MinSlot, _, MinSlot).
find_min_slot_helper([Slot|RestSlotList], WordList, CurrentMinSlot,
                     CurrentMinCount, MinSlot) :-
    count_matches(Slot, WordList, Count),
    (Count < CurrentMinCount ->
        % If matche count of current slot is lower, update min slot and count
        find_min_slot_helper(RestSlotList, WordList, Slot, Count, MinSlot)
    ;
        find_min_slot_helper(RestSlotList, WordList, CurrentMinSlot,
                             CurrentMinCount, MinSlot)
    ).

%% count_matches(+Slot:term, +WordList:list, -Count:int) is det.
%
%  Counts the number of words in WordList that match Slot.
%   - Slot: a list representing a slot.
%   - WordList: list of available words.
%   - Count: the total number of matches.
%
%  Example:
%    ?- count_matches([_, b, _], [[a,b,c], [a,c,c]], Count).
%    Count = 1.
%    ?- count_matches([a, _, c], [[a,b,c], [a,c,c]], Count).
%    Count = 2.
count_matches(Slot, WordList, Count) :-
    aggregate_all(count, (member(Word, WordList), Slot = Word), Count).

%% remove_first(+List:list, +Target:term, -Result:list) is det.
%
%  Remove the first specified element appeared in a List,
%  - List: original list.
%  - Target: specified element to be deleted.
%  - Result: list after the first specified element is removed.
%
%  Purpose: Compared to the build-in select/3, it is designed only for
%           delete the first match problem, no need to consider multiple
%           solutions. Faster than the more general select/3.
%
%  Example:
%    ?- remove_first([a, b, c, a], a, Result).
%    Result = [b, c, a].
remove_first([], _, []).
remove_first([E|Es], Target, Es) :-
    E == Target,
    % Prune ensure only the first match is removed.
    !.
remove_first([E|Es], Target, [E|Rs]) :- remove_first(Es, Target, Rs).
