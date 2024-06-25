use near_sdk::borsh::{BorshDeserialize, BorshSerialize};
use near_sdk::json_types::U128;
use near_sdk::serde::{Deserialize, Serialize};
use near_sdk::{env, log, near_bindgen, AccountId, PanicOnDefault};
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
    project_id_to_internal_id: HashMap<AccountId, u32>,
    internal_id_to_project_id: HashMap<u32, AccountId>,
    next_internal_id: u32,
}

const PICK_DELIMITER: &str = ":";

#[near_bindgen]
impl Contract {
    #[init]
    pub fn new() -> Self {
        Self {
            rounds_by_id: HashMap::new(),
            project_id_to_internal_id: HashMap::new(),
            internal_id_to_project_id: HashMap::new(),
            next_internal_id: 0,
        }
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

        if let Some(projects) = projects {
            for project in projects {
                let internal_id = self.next_internal_id;
                self.project_id_to_internal_id
                    .insert(project.clone(), internal_id);
                self.internal_id_to_project_id
                    .insert(internal_id, project.clone());
                round.approved_applicants.insert(project);
                self.next_internal_id += 1;
            }
        }

        self.rounds_by_id.insert(id, round);
        self.rounds_by_id.get(&id).unwrap()
    }

    /// Retrieve a round by its ID
    pub fn get_round(&self, round_id: RoundId) -> &Round {
        self.rounds_by_id.get(&round_id).expect("Round not found")
    }

    pub fn add_projects_to_round(&mut self, round_id: RoundId, projects: Vec<AccountId>) -> &Round {
        let caller = env::predecessor_account_id();
        let round = self
            .rounds_by_id
            .get_mut(&round_id)
            .expect("Round not found");

        // Verify caller is owner or admin
        if round.owner != caller && !round.admins.contains(&caller) {
            panic!("Only owner or admin can add projects to round");
        }

        for project in projects {
            let internal_id = self.next_internal_id;
            self.project_id_to_internal_id
                .insert(project.clone(), internal_id);
            self.internal_id_to_project_id
                .insert(internal_id, project.clone());
            round.approved_applicants.insert(project);
            self.next_internal_id += 1;
        }

        self.rounds_by_id.get(&round_id).unwrap()
    }

    pub fn submit_vote(&mut self, round_id: RoundId, picks: Vec<String>) {
        let voter_id = env::predecessor_account_id();
        let round = self
            .rounds_by_id
            .get_mut(&round_id)
            .expect("Round not found");

        let mut internal_picks = Vec::new();
        for pick in picks {
            let projects: Vec<&str> = pick.split(PICK_DELIMITER).collect();
            let project_a: AccountId = projects[0].parse().unwrap();
            let project_b: AccountId = projects[1].parse().unwrap();
            if projects.len() == 2 {
                let project_a_internal_id = self
                    .project_id_to_internal_id
                    .get(&project_a)
                    .expect("Invalid project ID")
                    .to_string();
                let project_b_internal_id = self
                    .project_id_to_internal_id
                    .get(&project_b)
                    .expect("Invalid project ID")
                    .to_string();
                internal_picks.push(format!(
                    "{}{}{}",
                    project_a_internal_id, PICK_DELIMITER, project_b_internal_id
                ));
            }
        }

        round.votes.insert(voter_id, internal_picks.join(","));
    }

    pub fn get_picks(&self, round_id: RoundId) -> HashMap<AccountId, Vec<String>> {
        let round = self.rounds_by_id.get(&round_id).expect("Round not found");
        let mut picks_map = HashMap::new();
        for (voter_id, picks_str) in &round.votes {
            let picks: Vec<String> = picks_str
                .split(',')
                .map(|s| {
                    let ids: Vec<&str> = s.split(PICK_DELIMITER).collect();
                    let project_a = self
                        .internal_id_to_project_id
                        .get(&ids[0].parse::<u32>().unwrap())
                        .expect("Invalid project ID");
                    let project_b = self
                        .internal_id_to_project_id
                        .get(&ids[1].parse::<u32>().unwrap())
                        .expect("Invalid project ID");
                    format!("{}{}{}", project_a, PICK_DELIMITER, project_b)
                })
                .collect();
            picks_map.insert(voter_id.clone(), picks);
        }
        picks_map
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

    pub fn get_projects_with_ids(&self, from_index: u32, limit: u32) -> Vec<(AccountId, u32)> {
        let start = (from_index * limit) as usize;
        let end = ((from_index + 1) * limit) as usize;
        self.project_id_to_internal_id
            .iter()
            .skip(start)
            .take(end - start)
            .map(|(project_id, internal_id)| (project_id.clone(), *internal_id))
            .collect()
    }

    pub fn get_internal_project_ids(&self) -> Vec<u32> {
        self.internal_id_to_project_id.keys().cloned().collect()
    }
}
