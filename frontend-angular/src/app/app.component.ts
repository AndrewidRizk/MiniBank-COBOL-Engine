import { Component } from '@angular/core'; 
import { DepositFormComponent } from './components/deposit-form/deposit-form.component';


@Component({
  selector: 'app-root',
  imports: [DepositFormComponent],
  templateUrl: './app.component.html',
  styleUrl: './app.component.css'
})
export class AppComponent {
  title = 'frontend-angular';
}
