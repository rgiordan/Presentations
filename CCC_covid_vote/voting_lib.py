import pandas as pd
import numpy as np

import copy
import itertools
import os
import re




POLICY_NAMES = ['Policy A', 'Policy B', 'Policy C', 'Policy D']

def PrintPolicyVec(vec, verbose=True):
    assert(len(vec) == len(POLICY_NAMES))
    if verbose:
        print(''.join([f'{POLICY_NAMES[i]:10}' for i in range(len(vec))]))
        print(''.join([f'{vec[i]:<10}' for i in range(len(vec))]))


######################
# Sanity checking

def AssertEquivalentVotes(old_vote, new_vote, exclude=np.array([])):
    # Assert that the strict pairwise ranks of the new_vote matches the old_vote,
    # excluding the indices in exclude.  This makes sure that I drop votes
    # or modify ill-formed votes correctly.

    # Though computatoinally wasteful, I'll use this in place of unit tests when
    # modifying the votes for ranked choice and resolving errors.

    assert(len(old_vote) == len(new_vote))

    num_policies = len(old_vote)
    if (len(exclude) > 0):
        assert(np.max(exclude) < num_policies)
        assert(np.min(exclude) >= 0)

    policies = np.setdiff1d(np.arange(num_policies), exclude)

    for i, j in itertools.product(policies, policies):
        if old_vote[i] < old_vote[j]:
            assert(new_vote[i] < new_vote[j])


######################
# Loading and preprocessing

def ConvertToNumericVote(pd_col):
    # Empty rows (not voted on) are storted as nan, not strings.   Replace them
    # with zeros so we can handle them gracefully in numpy.
    vote_str = pd_col.fillna('0').to_numpy()

    # Keep only the numbers from the response text.
    vote_str = np.array([ re.sub(r'[^0-9]', '', v) for v in vote_str ])

    return vote_str.astype('int')


def ConvertResponsesToNumericVotes(responses):
    # Convert repsonses in Pandas format to an array of numeric ranks.
    # Select the columns that compile the votes

    a_col = responses.columns[3]
    b_col = responses.columns[4]
    c_col = responses.columns[5]
    d_col = responses.columns[6]
    # print('\n'.join([a_col, b_col, c_col, d_col]))

    # Assert that I have selected the columns correctly
    assert(re.search("Policy A", a_col))
    assert(re.search("Policy B", b_col))
    assert(re.search("Policy C", c_col))
    assert(re.search("Policy D", d_col))
    original_votes = np.vstack([
        ConvertToNumericVote(responses[col]) for col in [ a_col, b_col, c_col, d_col ]
    ])
    # Assert that the A vote is the first row as expected as a sanity check.
    assert(np.all(ConvertToNumericVote(responses[a_col]) == original_votes[0, :]))

    return original_votes



def RepairVote(old_vote, verbose=False):
    # Check for valid input and correct voting errors, following the office hours document.
    # Returns a new set of rankings which is in order and has no gaps or duplicates.
    # For a description of what's going on, set verbose=True.

    def VerbosePrint(s):
        if verbose:
            print(s)

    vote = copy.copy(old_vote)
    VerbosePrint(f'Original vote: {old_vote}')
    num_votes = sum(vote > 0)
    rank = 1
    while rank <= num_votes:
        VerbosePrint(f'Rank {rank}.  Current vote: {vote}')
        if not np.any(vote == rank):
            # There is no policy with this rank; decrement the other votes and try again.
            VerbosePrint(f'Rank {rank} missing, decrementing other votes')
            dec_inds = np.logical_and(vote > rank, vote > 0)
            vote[dec_inds] = vote[dec_inds] - 1
        else:
            rank_inds = np.argwhere(vote == rank).flatten()
            if len(rank_inds) > 1:
                # This rank was duplicated.  Randomly split up this rank among the
                # policies.
                VerbosePrint(f'Rank {rank} duplicated, randomly splitting indices {rank_inds}')

                num_dups = len(rank_inds)
                new_ranks = np.random.choice(num_dups, num_dups, replace=False) + rank

                # Increment other votes to make room for num_dups - 1 extra votes
                dec_inds = np.logical_and(vote > rank, vote > 0)
                vote[dec_inds] = vote[dec_inds] + num_dups - 1
                vote[rank_inds] = new_ranks
                rank += num_dups - 1
            else:
                # This rank is okay, go to the next one.
                rank += 1

    VerbosePrint(f'Final vote:')
    PrintPolicyVec(vote, verbose=verbose)
    AssertEquivalentVotes(old_vote, vote)
    return vote


#############################
# Voting

def FindWinnerAndLoser(votes, verbose=False):
    # Identify a winner (if there is one) and the losers among the votes array.
    # Return (winner, loser), where both may be arrays in the case of a tie.

    def VerbosePrint(s):
        if verbose:
            print(s)

    num_voters = votes.shape[1]
    num_votes = np.sum(votes == 1)
    assert(num_votes <= num_voters)
    if num_votes != num_voters:
        VerbosePrint(f'This many voters did not vote this round: {num_voters - num_votes}')

    # If we drop a proposal that has no votes, it cannot possibly change the outcome.
    # Drop the proposal that has the fewest first choice votes and has at least some
    # votes.  Note that this is necessary because we are representing removing a vote
    # as non-voting.
    has_votes = np.sum(votes > 1, axis=1) > 0
    vote_count = np.sum(votes == 1, axis=1)
    VerbosePrint(f'\nNumber of first-place votes:')
    PrintPolicyVec(vote_count, verbose=verbose)

    min_count = np.min(vote_count[has_votes])
    loser = np.argwhere(
        np.logical_and(vote_count == min_count, has_votes)).flatten()

    if len(loser) > 1:
        VerbosePrint(f'There was a tie among losers: {loser}')
    vote_prop = np.round(vote_count / num_votes, 4)
    VerbosePrint(f'\nThe proportion of first votes was')
    PrintPolicyVec(vote_prop, verbose=verbose)
    majority = vote_prop >= 0.5
    if np.any(majority):
        winner = np.argwhere(majority).flatten()
        winner_txt = ' '.join([ POLICY_NAMES[i] for i in winner ])
        if (len(winner) > 1):
            VerbosePrint(f'\nThere was a tie among winners: {winner_txt}.')
        else:
            VerbosePrint(f'The winner was {winner_txt}.')
        return (winner, loser)
    else:
        VerbosePrint('\nThere was no majority.')
        return ([], loser)


def RemovePolicy(votes, drop_index):
    # Drop a policy from consideration, and decrement other so that they remain
    # valid votes.  The dropped vote will be ranked zero, as if the voter did
    # not vote on it in the survey.

    new_votes = copy.copy(votes)
    increment_cols = np.argwhere(new_votes[drop_index, :] == 1).flatten()
    new_votes[:, increment_cols] = new_votes[:, increment_cols] - 1
    new_votes[new_votes < 0] = 0
    new_votes[drop_index, :] = 0

    # Sanity check that we have not changed any preferences.
    for voter in range(votes.shape[1]):
        AssertEquivalentVotes(votes[:, voter], new_votes[:, voter], exclude=[drop_index])

    return new_votes


def PerformRankedChoiceVoting(votes, verbose=False):
    # Perform ranked-choice voting on an array of votes.

    def VerbosePrint(s):
        if verbose:
            print(s)

    done = False
    votes_copy = copy.copy(votes)
    while not done:
        VerbosePrint('\nChecking for a majority.')
        winner, loser = FindWinnerAndLoser(votes_copy, verbose=verbose)

        if len(winner) > 0:
            VerbosePrint(f'The winner is {winner}')
            done = True
        else:
            if len(loser) > 1:
                print(f'Randomly dropping a loser from {loser}')
                loser_ind = np.random.choice(len(loser), 1)
                loser = np.array([ loser[loser_ind] ])
            VerbosePrint(f'Removing {POLICY_NAMES[loser[0]]}')
            votes_copy = RemovePolicy(votes_copy, loser[0])
            #print(votes_copy.T) # Great for debugging
    return winner


def RunMultiRoundVoting(votes, verbose=True):
    # Run multiple rounds, eliminating the winner from each round to get
    # a ranked list of preferences.
    winners = []

    votes_copy = copy.copy(votes)
    for round_ind in range(4):
        print(f'\n======================================\nRound {round_ind}:')
        round_winners = PerformRankedChoiceVoting(votes_copy, verbose=verbose)
        winners.append(round_winners)
        winner_txt = [ POLICY_NAMES[i] for i in round_winners ]
        print(f'*** The winner of this round is {winner_txt}')
        for winner in round_winners:
            votes_copy = RemovePolicy(votes_copy, winner)

    print('The winners in order were: ')
    winners = np.array(winners).flatten()
    print([ POLICY_NAMES[i] for i in winners ])


def RunBordaCountVote(votes):
    # The Borda count is a one-liner, haha.
    # The lowest wins.  Note that missing votes are treated badly here,
    # as a missing vote gives a lower vote than is possible otherwise.
    # A real Borda count should not allow missing votes.
    return(np.sum(votes, axis=1))
