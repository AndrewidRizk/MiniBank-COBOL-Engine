
# MiniBank COBOL + Django + Angular Application

## Overview

MiniBank is a modern full-stack banking prototype that connects an old COBOL banking engine with a modern REST API (Django) and a professional front-end (Angular).

It demonstrates how legacy banking systems can be modernized and exposed to modern web applications and mobile apps through APIs.

---

## Architecture

```
COBOL Engine (MiniBank.cob)  <-- Indexed Files (.dat)
           ↑
        Run Script (run.bat)
           ↑
Django REST API (Python)
           ↑
Angular Front-End (Bank Portal)
```

---

## Use Case

A bank like **CIBC**, **TD**, **RBC**, or **Scotiabank** still uses COBOL as a core system.

With MiniBank:

✅ Expose COBOL core as REST API  
✅ Build modern front-end for bank staff  
✅ Allow external apps to interact with COBOL through API  
✅ Modernize step-by-step instead of rewriting everything

Example:

1️⃣ Front-End creates account → writes to COBOL index file  
2️⃣ Deposit API writes to file → triggers COBOL processing  
3️⃣ Front-End displays transactions by parsing COBOL output

---

## Features

### Core Banking (COBOL)

✅ Create account  
✅ Deposit / Withdraw  
✅ Process deposit file  
✅ Balance check  
✅ Display accounts  
✅ Display transactions  
✅ Delete account  
✅ Log transactions

### REST API (Django)

- `/api/deposit/` → Deposit money  
- `/api/withdraw/` → Withdraw money  
- `/api/create-account/` → Create new account  
- `/api/accounts/` → View accounts  
- `/api/transactions/` → View transactions  

### Front-End (Angular)

- Mobile-friendly bank portal  
- Sidebar layout (New User / Moving Money / Administration)  
- Clean modern UI (inspired by CIBC / TD)  
- Live account display  
- Live transaction history  
- Responsive (hamburger menu for mobile)

---

## How to Run

### 1️⃣ Prerequisites

- Python 3.x  
- Django  
- GnuCOBOL  
- Angular CLI  
- Node.js / npm  
- MSYS2 + Mingw64 (Windows)

### 2️⃣ Clone Repo

```bash
git clone https://github.com/YOUR-REPO/MiniBank-COBOL-Engine.git
cd MiniBank-COBOL-Engine
```

### 3️⃣ Backend Setup

```bash
cd backend
python -m venv venv
venv\Scripts\activate
pip install -r requirements.txt
python manage.py migrate
python manage.py runserver
```

### 4️⃣ Front-End Setup

```bash
cd frontend-angular
npm install
ng serve --open
```

### 5️⃣ Running COBOL Engine

- Manual: `run.bat`
- Automatic: triggered from API when deposit/withdraw

---

## File Structure

```
MiniBank-COBOL-Engine/
├── src/
│   └── minibank.cob
├── data/
│   ├── accounts.dat
│   ├── static.dat
│   ├── deposit_request.txt
│   ├── transaction.dat
├── api/
│   ├── views.py
│   ├── urls.py
│   ├── run_cobol.py
├── run.bat
├── frontend-angular/
│   ├── components/
│   ├── app-routing.module.ts
│   ├── styles.css
│   ├── sidebar-layout.component.html
└── README.md
```

---

## Future Improvements

✅ Authentication (Django Rest Auth / JWT)  
✅ Better transaction audit  
✅ PDF statement export  
✅ Multi-user login  
✅ Admin dashboard

---

## Screenshots

(Add screenshots of **Accounts**, **Transactions**, **Mobile view**, **COBOL console**)

---

## Conclusion

MiniBank shows how to bridge the gap between:

- **Legacy COBOL engines**  
- **Modern REST APIs**  
- **Modern Angular UIs**

Banks don't need to "rip and replace" — they can modernize step-by-step.

---

**Built with ❤️ by Andro Rizk**

