import { Component } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
@Component({
  selector: 'app-withdraw-form',
  templateUrl: './withdraw-form.component.html',
  standalone: true,
  imports: [CommonModule, FormsModule]
})
export class WithdrawFormComponent {
  account_id = '';
  amount = 0;
  message = '';

  constructor(private http: HttpClient) {}

  submitWithdraw() {
    this.http.post('http://127.0.0.1:8000/api/withdraw/', {
      account_id: this.account_id,
      amount: this.amount
    }).subscribe({
      next: (response: any) => this.message = response.message,
      error: (err) => this.message = 'Error: ' + (err.error?.error || err.message)
    });
  }
}
