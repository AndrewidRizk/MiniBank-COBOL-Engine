import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
@Component({
  selector: 'app-accounts-view',
  templateUrl: './accounts-view.component.html',
  standalone: true,
  imports: [CommonModule, FormsModule]
})
export class AccountsViewComponent implements OnInit {
  accounts: any[] = [];

  constructor(private http: HttpClient) {}

  ngOnInit() {
this.http.get<any>('http://127.0.0.1:8000/api/accounts/')
    .subscribe(data => {
      this.accounts = data.accounts;  // not 'data' directly!
    });
  }
}
