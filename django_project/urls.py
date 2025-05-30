# django_project/urls.py

from django.contrib import admin
from django.urls import path, include

urlpatterns = [
    path('admin/', admin.site.urls),
    path('', include('banking_api.urls')),  # change 'banking_api' to your app name!
]
