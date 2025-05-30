import { Component } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-create-account-form',
  templateUrl: './create-account-form.component.html',
  standalone: true,
  imports: [FormsModule, CommonModule]
})
export class CreateAccountFormComponent {
  name = '';
  message = '';

  constructor(private http: HttpClient) {}

  createAccount() {
    this.http.post('http://127.0.0.1:8000/api/create-account/', {
      name: this.name
    }).subscribe({
      next: (response: any) => this.message = response.message,
      error: (err) => this.message = 'Error: ' + (err.error?.error || err.message)
    });
  }
}
