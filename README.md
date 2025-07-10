# 🔐 Cryptofuse - Encrypted Message Bounties

> **Reward decrypting secure info post-deadline** 🎯

Cryptofuse is a Stacks smart contract that enables users to create encrypted message bounties with time-locked rewards. Create puzzles, set deadlines, and reward those who can decrypt your messages after the deadline passes!

## 🚀 Features

- 📝 **Create Encrypted Bounties**: Post encrypted messages with STX rewards
- ⏰ **Time-Locked Solutions**: Solutions can only be submitted after deadline
- 🎁 **Automatic Rewards**: Successful solvers receive STX payouts
- 🔍 **Solution Verification**: SHA256 hash verification for correct solutions  
- 📊 **Bounty Management**: Cancel, extend deadlines, or increase rewards
- 💰 **Fee System**: Small contract fee for platform sustainability

## 🛠️ How It Works

1. **Create a Bounty** 🎯
   - Encrypt your message
   - Generate SHA256 hash of the solution
   - Set deadline and reward amount
   - Lock STX tokens in contract

2. **Wait for Deadline** ⏳
   - Solutions cannot be submitted before deadline
   - Bounty creator can cancel or modify before deadline

3. **Submit Solutions** 🔓
   - After deadline, anyone can attempt to solve
   - Submit plain text solution for verification
   - First correct solution wins the reward

## 📋 Usage Instructions

### Creating a Bounty

```clarity
(contract-call? .Cryptofuse create-bounty 
  "U2FsdGVkX1+encrypted+message+here"  ;; encrypted message
  0x1234567890abcdef...                 ;; solution hash (SHA256)
  u1000                                 ;; deadline (block height)
  u1000000)                            ;; reward (microSTX)
```

### Submitting a Solution

```clarity
(contract-call? .Cryptofuse submit-solution 
  u1                    ;; bounty ID
  "my secret answer")   ;; solution attempt
```

### Managing Your Bounties

```clarity
;; Cancel bounty (before deadline)
(contract-call? .Cryptofuse cancel-bounty u1)

;; Extend deadline
(contract-call? .Cryptofuse extend-deadline u1 u2000)

;; Increase reward
(contract-call? .Cryptofuse increase-bounty-reward u1 u500000)
```

## 🔍 Read-Only Functions

- `get-bounty` - Get bounty details
- `get-bounty-count` - Total bounties created
- `get-user-bounties` - User's bounty list
- `is-bounty-active` - Check if bounty is active
- `is-bounty-solvable` - Check if bounty can be solved
- `calculate-solution-hash` - Generate hash for solutions

## 💡 Example Workflow

1. **Alice** wants to create a puzzle bounty 🧩
2. She encrypts "The answer is 42" → gets encrypted string
3. She calculates SHA256("The answer is 42") → gets hash
4. Creates bounty with 1 STX reward, deadline in 100 blocks
5. **Bob** sees the bounty after deadline passes
6. Bob decrypts the message and submits "The answer is 42"
7. Contract verifies hash matches and pays Bob 1 STX! 💰

## ⚠️ Important Notes

- Solutions can only be submitted **after** the deadline
- Only the **first correct solution** wins the reward
- Contract takes a small fee (2.5% default) from rewards
- Bounty creators can cancel before deadline to get refund
- All STX amounts are in microSTX (1 STX = 1,000,000 microSTX)

## 🔧 Development

Built with Clarinet for the Stacks blockchain. Deploy using:

```bash
clarinet deploy
```

## 🎮 Use Cases

- 🎓 **Educational Puzzles**: Create learning challenges with rewards
- 🕵️ **Mystery Games**: ARGs and treasure hunts
- 🔐 **Security Challenges**: Test cryptographic skills
- 📚 **Research Bounties**: Incentivize problem solving
- 🎪 **Entertainment**: Fun community challenges

---

*Happy bounty hunting! 🏆*
```


