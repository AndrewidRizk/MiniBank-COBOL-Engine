// src/app/app.routes.ts

import { Routes } from '@angular/router';
import { DepositFormComponent } from './components/deposit-form/deposit-form.component';
import { WithdrawFormComponent } from './components/withdraw-form/withdraw-form.component';
import { CreateAccountFormComponent } from './components/create-account-form/create-account-form.component';
import { AccountsViewComponent } from './components/accounts-view/accounts-view.component';
import { TransactionsViewComponent } from './components/transactions-view/transactions-view.component';

export const appRoutes: Routes = [
  { path: '', redirectTo: '/deposit', pathMatch: 'full' },
  { path: 'deposit', component: DepositFormComponent },
  { path: 'withdraw', component: WithdrawFormComponent },
  { path: 'create-account', component: CreateAccountFormComponent },
  { path: 'accounts', component: AccountsViewComponent },
  { path: 'transactions', component: TransactionsViewComponent }
];
