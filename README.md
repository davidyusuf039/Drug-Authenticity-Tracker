# Drug Authenticity Tracker Smart Contract

A Clarity smart contract for tracking pharmaceutical drugs through the supply chain on the Stacks blockchain.

## Features

- Register new drug batches with manufacturing details
- Transfer ownership of drug batches between supply chain participants
- Confirm receipt of drug shipments
- Track complete transfer history of each batch
- Query batch details and transfer history

## Contract Functions

### Public Functions

1. `register-drug-batch`: Register a new drug batch (manufacturer only)
2. `transfer-batch`: Transfer a batch to another participant
3. `confirm-receipt`: Confirm receipt of a transferred batch

### Read-Only Functions

1. `get-batch-details`: Get details of a specific batch
2. `get-transfer-history`: Get transfer history entry
3. `get-batch-transfer-count`: Get total transfers for a batch

## Usage Example

```clarity
;; Register a new drug batch
(contract-call? .drug-auth register-drug-batch "Aspirin" u1677852800 u1709388800)

;; Transfer batch to distributor
(contract-call? .drug-auth transfer-batch u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 "New York Warehouse")

;; Confirm receipt
(contract-call? .drug-auth confirm-receipt u1)
```

## Data Structure

- Drug batches are tracked using unique batch IDs
- Each transfer is recorded with timestamp and location
- Status tracking: manufactured → in-transit → received
```
