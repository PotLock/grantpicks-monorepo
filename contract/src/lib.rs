use near_sdk::borsh::{self, BorshDeserialize, BorshSerialize};
use near_sdk::json_types::U128;
use near_sdk::serde::{Deserialize, Serialize};
use near_sdk::{env, log, near_bindgen, AccountId, PanicOnDefault};
use num_bigint::BigUint;
use num_traits::{ToPrimitive, Zero};
use std::collections::{HashMap, HashSet};

pub type TimestampMs = u64;
pub type RoundId = u64;

#[derive(BorshDeserialize, BorshSerialize, Serialize, Deserialize)]
#[borsh(crate = "near_sdk::borsh")]
#[serde(crate = "near_sdk::serde")]
pub struct Contact {
    id: String,
    value: String,
}

#[derive(BorshDeserialize, BorshSerialize, Serialize, Deserialize)]
#[borsh(crate = "near_sdk::borsh")]
#[serde(crate = "near_sdk::serde")]
pub struct Requirement {
    id: String,
    value: String,
}

#[derive(BorshDeserialize, BorshSerialize, Serialize, Deserialize)]
#[borsh(crate = "near_sdk::borsh")]
#[serde(crate = "near_sdk::serde")]
pub struct Payout {
    amount: String,
    paid_at_ms: TimestampMs,
    error: Option<String>,
}

#[derive(BorshDeserialize, BorshSerialize, PartialEq, Debug, Clone, Serialize, Deserialize)]
#[borsh(crate = "near_sdk::borsh")]
#[serde(crate = "near_sdk::serde")]
pub enum ApplicationStatus {
    Pending,
    Approved,
    Rejected,
    InReview,
}

#[derive(BorshDeserialize, BorshSerialize, Serialize, Deserialize)]
#[borsh(crate = "near_sdk::borsh")]
#[serde(crate = "near_sdk::serde")]
pub struct Application {
    pub message: Option<String>,
    pub status: ApplicationStatus,
    pub submitted_at: TimestampMs,
    pub updated_at: Option<TimestampMs>,
    pub review_notes: Option<String>,
}

#[derive(BorshDeserialize, BorshSerialize, Serialize, Deserialize)]
#[borsh(crate = "near_sdk::borsh")]
#[serde(crate = "near_sdk::serde")]
pub struct Round {
    id: RoundId,
    owner: AccountId,
    admins: Vec<AccountId>,
    application_start_ms: TimestampMs,
    application_end_ms: TimestampMs,
    voting_start_ms: TimestampMs,
    voting_end_ms: TimestampMs,
    blacklisted_voters: Vec<AccountId>,
    whitelisted_voters: Option<Vec<AccountId>>,
    expected_amount: U128,
    vault_balance: U128,
    name: String,
    description: Option<String>,
    contacts: Vec<Contact>,
    image_url: Option<String>,
    application_questions: Vec<String>,
    application_requirements: Vec<Requirement>,
    voting_requirements: Vec<Requirement>,
    num_picks_per_voter: u8,
    applications: HashMap<AccountId, Application>,
    approved_applicants: HashSet<AccountId>,
    votes: HashMap<AccountId, String>,
    payouts: HashMap<AccountId, Payout>,
}

#[near_bindgen]
#[derive(BorshDeserialize, BorshSerialize, PanicOnDefault)]
#[borsh(crate = "near_sdk::borsh")]
pub struct Contract {
    rounds_by_id: HashMap<u64, Round>,
}

const PICK_DELIMITER: &str = ":";

#[near_bindgen]
impl Contract {
    #[init]
    pub fn new() -> Self {
        Self {
            rounds_by_id: HashMap::new(),
        }
    }

    /// Encode preferences into a BigUint using 2 bits per pick
    pub(crate) fn encode_preferences(pairs: Vec<(usize, bool)>, total_pairs: usize) -> BigUint {
        let mut encoded = BigUint::zero();
        let mut voted_pairs = vec![false; total_pairs];
        for (index, pref) in pairs {
            let bit_position = index * 2;
            let bit_value = if pref { 1u8 } else { 0u8 };
            encoded |= BigUint::from(bit_value) << bit_position;
            voted_pairs[index] = true;
        }
        // Set all unvoted pairs to a specific value (e.g., 3)
        for (i, voted) in voted_pairs.iter().enumerate() {
            if !voted {
                encoded |= BigUint::from(3u8) << (i * 2);
            }
        }
        encoded
    }

    /// Decode preferences from a BigUint
    pub(crate) fn decode_preferences(encoded: BigUint, total_pairs: usize) -> Vec<(usize, bool)> {
        let mut pairs = Vec::new();
        for i in 0..total_pairs {
            let bit_position = i * 2;
            let bit_value = (&encoded >> bit_position) & BigUint::from(3u8);
            if bit_value == BigUint::from(0u8) || bit_value == BigUint::from(1u8) {
                let pref = bit_value == BigUint::from(1u8);
                pairs.push((i, pref));
            }
        }
        pairs
    }

    /// Submit a vote by a voter with their picks
    pub fn submit_vote(&mut self, round_id: RoundId, picks: Vec<String>) {
        let initial_storage_usage = env::storage_usage();
        let voter_id = env::predecessor_account_id();
        // First, get approved_applicants immutably
        let approved_applicants: Vec<AccountId> = {
            let round = self.rounds_by_id.get(&round_id).expect("Round not found");
            round.approved_applicants.iter().cloned().collect()
        };
        // Calculate total pairs outside of the mutable borrow
        let total_pairs = approved_applicants.len() * (approved_applicants.len() - 1) / 2;
        let pairs = self.convert_picks_to_pairs(picks.clone(), &approved_applicants);
        let encoded_preferences = Self::encode_preferences(pairs, total_pairs);
        // Convert BigUint to string
        let encoded_preferences_str = encoded_preferences.to_str_radix(10);
        // Now, mutably borrow the round
        let round = self
            .rounds_by_id
            .get_mut(&round_id)
            .expect("Round not found");
        round
            .votes
            .insert(voter_id.clone(), encoded_preferences_str);
        // calculate & log storage usage
        let storage_usage = env::storage_usage() - initial_storage_usage;
        log!(
            "Storage used for vote by {}: {} bytes",
            voter_id,
            storage_usage
        );
    }

    /// Get a random number using `env::random_seed` and a shift amount.
    pub(crate) fn get_random_number(shift_amount: u32) -> u64 {
        let seed = env::random_seed();
        let timestamp = env::block_timestamp().to_le_bytes();

        // Prepend the timestamp to the seed
        let mut new_seed = Vec::with_capacity(seed.len() + timestamp.len());
        new_seed.extend_from_slice(&timestamp);
        new_seed.extend_from_slice(&seed);

        // Rotate new_seed
        let len = new_seed.len();
        new_seed.rotate_left(shift_amount as usize % len);

        // Copy to array and convert to u64
        let mut arr: [u8; 8] = Default::default();
        arr.copy_from_slice(&new_seed[..8]);
        u64::from_le_bytes(arr)
    }

    /// Get random pairs for voting
    pub fn get_random_picks(&self, round_id: RoundId) -> Vec<String> {
        let round = self.rounds_by_id.get(&round_id).expect("Round not found");
        let approved_applicants: Vec<AccountId> =
            round.approved_applicants.iter().cloned().collect();
        let all_pairs = self.generate_all_pairs(&approved_applicants);
        let num_picks = round.num_picks_per_voter as usize;
        let mut selected_pairs = Vec::new();
        let mut used_indices = std::collections::HashSet::new();

        for i in 0..num_picks {
            let random_num = Self::get_random_number(i as u32);
            let mut index = (random_num % all_pairs.len() as u64) as usize;

            // Ensure the same pair is not picked more than once
            while used_indices.contains(&index) {
                index = (index + 1) % all_pairs.len();
            }

            used_indices.insert(index);
            selected_pairs.push(all_pairs[index].clone());
        }

        selected_pairs
    }

    /// Retrieve and decode a voter's picks
    pub fn get_decoded_picks(&self, round_id: RoundId) -> HashMap<AccountId, Vec<String>> {
        let round = self.rounds_by_id.get(&round_id).expect("Round not found");
        let approved_applicants: Vec<AccountId> =
            round.approved_applicants.iter().cloned().collect();
        let num_pairs = approved_applicants.len() * (approved_applicants.len() - 1) / 2;
        let mut decoded_picks_map = HashMap::new();

        for (voter_id, encoded_str) in &round.votes {
            let encoded =
                BigUint::parse_bytes(encoded_str.as_bytes(), 10).expect("Invalid BigUint string");
            let decoded_picks = Self::decode_preferences(encoded, num_pairs);

            let decoded_pick_strings: Vec<String> = decoded_picks
                .iter()
                .map(|(index, pref)| {
                    let (project_a_index, project_b_index) =
                        self.get_project_pair_indices(*index, &approved_applicants);
                    let project_a = &approved_applicants[project_a_index];
                    let project_b = &approved_applicants[project_b_index];
                    if *pref {
                        format!("{}{}{}", project_a, PICK_DELIMITER, project_b)
                    } else {
                        format!("{}{}{}", project_b, PICK_DELIMITER, project_a)
                    }
                })
                .collect();

            decoded_picks_map.insert(voter_id.clone(), decoded_pick_strings);
        }

        decoded_picks_map
    }

    /// Generate all possible pairs from a list of projects
    pub(crate) fn generate_all_pairs(&self, projects: &[AccountId]) -> Vec<String> {
        let mut pairs = Vec::new();
        for i in 0..projects.len() {
            for j in (i + 1)..projects.len() {
                pairs.push(format!("{}{}{}", projects[i], PICK_DELIMITER, projects[j]));
            }
        }
        pairs
    }

    /// Helper function to get project pair indices from a linear index
    pub(crate) fn get_project_pair_indices(
        &self,
        index: usize,
        projects: &[AccountId],
    ) -> (usize, usize) {
        let mut count = 0;
        for i in 0..projects.len() {
            for j in (i + 1)..projects.len() {
                if count == index {
                    return (i, j);
                }
                count += 1;
            }
        }
        panic!("Index out of bounds");
    }

    /// Convert picks from project ID pairs to index pairs with preferences
    fn convert_picks_to_pairs(
        &self,
        picks: Vec<String>,
        projects: &[AccountId],
    ) -> Vec<(usize, bool)> {
        picks
            .iter()
            .map(|pick| {
                let mut split = pick.split(PICK_DELIMITER);
                let chosen_project = split.next().unwrap_or_default();
                let other_project = split.next().unwrap_or_default();
                let chosen_index = projects
                    .iter()
                    .position(|p| p == chosen_project)
                    .expect("Invalid project ID");
                let other_index = projects
                    .iter()
                    .position(|p| p == other_project)
                    .expect("Invalid project ID");
                if chosen_index < other_index {
                    (
                        chosen_index * projects.len() + other_index
                            - (chosen_index + 1) * (chosen_index + 2) / 2,
                        true,
                    )
                } else {
                    (
                        other_index * projects.len() + chosen_index
                            - (other_index + 1) * (other_index + 2) / 2,
                        false,
                    )
                }
            })
            .collect()
    }

    /// Retrieve a round by its ID
    pub fn get_round(&self, round_id: RoundId) -> &Round {
        self.rounds_by_id.get(&round_id).expect("Round not found")
    }

    /// Calculate aggregated preferences from all votes
    pub fn calculate_preferences_for_round(&self, round: &Round) -> Vec<(usize, usize, bool)> {
        let num_pairs = round.approved_applicants.len() * (round.approved_applicants.len() - 1) / 2;
        let mut aggregated_preferences = vec![0; num_pairs];
        for encoded_str in round.votes.values() {
            let encoded =
                BigUint::parse_bytes(encoded_str.as_bytes(), 10).expect("Invalid BigUint string");
            for i in 0..num_pairs {
                let bit_position = i * 2;
                let bit_value = (&encoded >> bit_position) & BigUint::from(1u8);
                aggregated_preferences[i] += bit_value.to_usize().unwrap_or(0);
            }
        }
        aggregated_preferences
            .iter()
            .enumerate()
            .map(|(i, &count)| (i, num_pairs - i - 1, count > 0))
            .collect()
    }

    pub fn create_round(
        &mut self,
        owner: Option<AccountId>,
        admins: Option<Vec<AccountId>>,
        application_start_ms: TimestampMs,
        application_end_ms: TimestampMs,
        voting_start_ms: TimestampMs,
        voting_end_ms: TimestampMs,
        blacklisted_voters: Option<Vec<AccountId>>,
        whitelisted_voters: Option<Vec<AccountId>>,
        expected_amount: U128,
        name: String,
        description: Option<String>,
        contacts: Vec<Contact>,
        image_url: Option<String>,
        application_questions: Option<Vec<String>>,
        application_requirements: Option<Vec<Requirement>>,
        voting_requirements: Option<Vec<Requirement>>,
        num_picks_per_voter: u8,
        projects: Option<Vec<AccountId>>,
    ) -> &Round {
        let id = (self.rounds_by_id.len() + 1) as u64;
        let mut round = Round {
            id,
            owner: owner.unwrap_or_else(|| env::predecessor_account_id()),
            admins: admins.unwrap_or_else(|| vec![]),
            application_start_ms,
            application_end_ms,
            voting_start_ms,
            voting_end_ms,
            blacklisted_voters: blacklisted_voters.unwrap_or_else(|| vec![]),
            whitelisted_voters,
            expected_amount,
            vault_balance: U128(0),
            name,
            description,
            contacts,
            image_url,
            application_questions: application_questions.unwrap_or_else(|| vec![]),
            application_requirements: application_requirements.unwrap_or_else(|| vec![]),
            voting_requirements: voting_requirements.unwrap_or_else(|| vec![]),
            num_picks_per_voter,
            applications: HashMap::new(),
            approved_applicants: HashSet::new(),
            votes: HashMap::new(),
            payouts: HashMap::new(),
        };
        // add projects to approved_applicants
        if let Some(projects) = projects {
            for project in projects {
                round.approved_applicants.insert(project);
            }
        }
        self.rounds_by_id.insert(id, round);
        self.rounds_by_id.get(&id).unwrap()
    }

    pub fn add_projects_to_round(&mut self, round_id: RoundId, projects: Vec<AccountId>) -> &Round {
        let round = self
            .rounds_by_id
            .get_mut(&round_id)
            .expect("Round not found");
        let caller = env::predecessor_account_id();
        // Verify caller is owner or admin
        if round.owner != caller && !round.admins.contains(&caller) {
            panic!("Only owner or admin can add projects to round");
        }
        // Add projects to HashSet to ensure uniqueness
        for project in projects {
            round.approved_applicants.insert(project);
        }
        round
    }
}
