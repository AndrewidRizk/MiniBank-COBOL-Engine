import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonModule } from '@angular/common';
@Component({
  selector: 'app-transactions-view',
  templateUrl: './transactions-view.component.html',
  standalone: true,
  imports: [CommonModule]
})
export class TransactionsViewComponent implements OnInit {
  transactions: any[] = [];

  constructor(private http: HttpClient) {}

  ngOnInit() {
this.http.get<any>('http://127.0.0.1:8000/api/transactions/')
    .subscribe(data => {
      this.transactions = data.transactions;
    });
  }
}
