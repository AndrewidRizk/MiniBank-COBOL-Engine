import { Component } from '@angular/core';
import { SidebarLayoutComponent } from './components/sidebar-layout/sidebar-layout.component';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [SidebarLayoutComponent],
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'MiniBank';
}
