# 🏆 Servirep - Service Review Token

> 🚀 Blockchain-powered service reviews with NFT verification

## 📋 Overview

Servirep is a revolutionary Clarity smart contract that creates **Service Review Tokens** - verified reviews linked to NFTs on the Stacks blockchain. Service providers can mint NFTs representing their services, while customers leave authenticated, immutable reviews that build trust and transparency in the service economy.

## ✨ Key Features

- 🎫 **NFT Service Tokens**: Each service is represented by a unique NFT
- ⭐ **Verified Reviews**: Immutable, blockchain-verified customer feedback
- 💰 **Integrated Payments**: Direct STX payments with platform fees
- 🗳️ **Community Voting**: Users can vote on helpful reviews
- 💡 **Tip System**: Reward quality reviewers with STX tips
- 🏷️ **Categorization**: Organize services by category
- 📊 **Rating Analytics**: Automatic average rating calculations

## 🛠️ Smart Contract Functions

### Service Management

#### `create-service`
```clarity
(create-service "Web Design" "Professional website creation" "Design" u500000)
```
Creates a new service NFT with specified details and pricing.

#### `update-service`
```clarity
(update-service u1 "Updated Service" "New description" u600000)
```
Updates service information (owner only).

#### `toggle-service-status`
```clarity
(toggle-service-status u1)
```
Activates or deactivates a service.

### Review System

#### `submit-review`
```clarity
(submit-review u1 u5 "Excellent service, highly recommended!")
```
Submit a review with rating (1-5) and comment.

#### `verify-review`
```clarity
(verify-review u1)
```
Verify a review (contract owner only).

#### `vote-helpful`
```clarity
(vote-helpful u1)
```
Vote a review as helpful.

### Payment & Tips

#### `purchase-service`
```clarity
(purchase-service u1)
```
Purchase a service with STX payment.

#### `tip-reviewer`
```clarity
(tip-reviewer u1 u100000)
```
Tip a reviewer in STX.

## 📖 Usage Examples

### 1. Service Provider Workflow 🏪

```bash
# Deploy contract and create service
clarinet console
(contract-call? .Servirep create-service "Logo Design" "Custom logo creation" "Design" u250000)

# Update service details
(contract-call? .Servirep update-service u1 "Premium Logo Design" "High-end custom logos" u350000)

# Deactivate service temporarily
(contract-call? .Servirep toggle-service-status u1)
```

### 2. Customer Workflow 👥

```bash
# Purchase a service
(contract-call? .Servirep purchase-service u1)

# Leave a review
(contract-call? .Servirep submit-review u1 u4 "Great work but took longer than expected")

# Vote on helpful reviews
(contract-call? .Servirep vote-helpful u1)

# Tip a quality reviewer
(contract-call? .Servirep tip-reviewer u1 u50000)
```

### 3. Data Queries 📊

```bash
# Get service details
(contract-call? .Servirep get-service u1)

# Check service reviews
(contract-call? .Servirep get-service-reviews u1)

# View user's reviews
(contract-call? .Servirep get-user-reviews 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE)

# Check if user already reviewed
(contract-call? .Servirep has-user-reviewed 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE u1)
```

## 🏗️ Development Setup

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd servirep
   ```

2. **Run tests**
   ```bash
   clarinet test
   ```

3. **Start console**
   ```bash
   clarinet console
   ```

4. **Deploy locally**
   ```bash
   clarinet integrate
   ```

## 🔒 Security Features

- ✅ **Authorization checks** for all critical functions
- ✅ **Input validation** for ratings and payments
- ✅ **Duplicate review prevention** per user per service
- ✅ **Fee cap protection** (max 10% platform fee)
- ✅ **Service status validation** before purchases

## 📊 Error Codes

| Code | Description |
|------|-------------|
| `u401` | Not authorized |
| `u404` | Service not found |
| `u405` | Review not found |
| `u406` | Already reviewed |
| `u407` | Invalid rating |
| `u408` | Service inactive |
| `u409` | Insufficient payment |

## 🌟 Advanced Features

### Category Filtering
```clarity
(get-services-by-category "Design" u10)
```

### Top Rated Services
```clarity
(get-top-rated-services u5)
```

### Platform Fee Management
```clarity
(set-platform-fee u2000) ; 2% fee
```

## 🚀 Deployment

### Testnet Deployment
```bash
clarinet publish --testnet
```

### Mainnet Deployment
```bash
clarinet publish --mainnet
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Submit a pull request

## 📄 License

MIT License - see LICENSE file for details.

## 🔗 Links

- [Stacks Documentation](https://docs.stacks.co/)
- [Clarity Language Reference](https://docs.stacks.co/docs/clarity/)
- [Clarinet Documentation](https://github.com/hirosystems/clarinet)


