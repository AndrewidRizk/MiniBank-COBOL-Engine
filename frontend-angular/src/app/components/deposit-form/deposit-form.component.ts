import { Component } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { FormsModule } from '@angular/forms';  // ✅ Add this!
import { CommonModule } from '@angular/common';
@Component({
  selector: 'app-deposit-form',
  templateUrl: './deposit-form.component.html',
  standalone: true,
  imports: [CommonModule, FormsModule]
})
export class DepositFormComponent {
  account_id = '';
  amount = 0;
  message = '';

  constructor(private http: HttpClient) {}

  submitDeposit() {
    this.http.post('http://127.0.0.1:8000/api/deposit/', {
      account_id: this.account_id,
      amount: this.amount
    }).subscribe({
      next: (response: any) => this.message = response.message,
      error: (err) => this.message = 'Error: ' + (err.error?.error || err.message)
    });
  }
}
