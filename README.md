# 💉 Vaccine Distribution Smart Contract System

A blockchain-based vaccine distribution system that ensures verifiable cold-chain delivery and tracking of vaccines from production to administration.

## 🎯 Overview

This smart contract system provides a complete solution for tracking vaccines through the entire supply chain, ensuring cold-chain compliance and preventing counterfeit vaccines. The system maintains an immutable record of each vaccine's journey from manufacturer to patient.

## ✨ Features

- 🏭 **Batch Management**: Create and manage vaccine batches with production details
- 💉 **Individual Vaccine Tracking**: Track each vaccine unit with unique ID
- 🌡️ **Cold-Chain Monitoring**: Real-time temperature tracking and violation detection
- 🏥 **Facility Authorization**: Role-based access control for different facility types
- 📋 **Complete Audit Trail**: Immutable history of vaccine lifecycle
- 🔒 **Secure Administration**: Prevent administration of compromised vaccines
- 📊 **Verification System**: Easy verification of vaccine authenticity and cold-chain compliance

## 🏗️ Contract Structure

### Core Data Types

- **Vaccines**: Individual vaccine units with temperature history
- **Batches**: Production batches with specifications
- **Facilities**: Authorized participants in the supply chain
- **Temperature Logs**: Historical temperature readings
- **Permissions**: Role-based access control

### Facility Types & Permissions

- **Manufacturers**: Can create batches and produce vaccines
- **Distributors**: Can transfer vaccines and update temperatures
- **Healthcare Providers**: Can administer vaccines to patients

## 🚀 Usage Instructions

### 1. Deploy the Contract

```bash
clarinet deploy
```

### 2. Authorize Facilities

First, authorize facilities that will participate in the vaccine distribution:

```clarity
(contract-call? .vaccine-distribution authorize-facility 
  'ST1MANUFACTURER 
  "Pharma Corp" 
  "manufacturer" 
  true false false)

(contract-call? .vaccine-distribution authorize-facility 
  'ST1DISTRIBUTOR 
  "Cold Chain Logistics" 
  "distributor" 
  false true false)

(contract-call? .vaccine-distribution authorize-facility 
  'ST1HOSPITAL 
  "City Hospital" 
  "healthcare" 
  false false true)
```

### 3. Create Vaccine Batch

Manufacturers create batches:

```clarity
(contract-call? .vaccine-distribution create-batch 
  "COVID-19 mRNA" 
  u1000 
  u1000000 
  -70 
  -60)
```

### 4. Produce Individual Vaccines

Create individual vaccine units from batches:

```clarity
(contract-call? .vaccine-distribution produce-vaccine 
  u1 
  "Manufacturing Facility A" 
  -65)
```

### 5. Track Temperature During Distribution

Update temperature readings during transport:

```clarity
(contract-call? .vaccine-distribution update-temperature 
  u1 
  -68 
  "Distribution Center")
```

### 6. Transfer Between Facilities

Transfer vaccines through the supply chain:

```clarity
(contract-call? .vaccine-distribution transfer-vaccine 
  u1 
  'ST1HOSPITAL 
  "City Hospital Pharmacy")
```

### 7. Administer Vaccine

Healthcare providers administer vaccines to patients:

````clarity
(contract-call? .vaccine-distribution administer-
