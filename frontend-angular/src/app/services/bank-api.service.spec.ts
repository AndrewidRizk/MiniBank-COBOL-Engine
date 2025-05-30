import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class BankApiService {
  private apiUrl = 'http://127.0.0.1:8000/api';

  constructor(private http: HttpClient) { }

  deposit(account_id: string, amount: number): Observable<any> {
    return this.http.post(`${this.apiUrl}/deposit/`, { account_id, amount });
  }

  withdraw(account_id: string, amount: number): Observable<any> {
    return this.http.post(`${this.apiUrl}/withdraw/`, { account_id, amount });
  }

  createAccount(name: string): Observable<any> {
    return this.http.post(`${this.apiUrl}/create-account/`, { name });
  }

  getAccounts(): Observable<any> {
    return this.http.get(`${this.apiUrl}/accounts/`);
  }

  getAccountBalance(account_id: string): Observable<any> {
    return this.http.get(`${this.apiUrl}/account/${account_id}/balance/`);
  }

  getTransactions(): Observable<any> {
    return this.http.get(`${this.apiUrl}/transactions/`);
  }

  getAccountTransactions(account_id: string): Observable<any> {
    return this.http.get(`${this.apiUrl}/account/${account_id}/transactions/`);
  }
}
