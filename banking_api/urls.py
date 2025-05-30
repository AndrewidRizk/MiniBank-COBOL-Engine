from django.urls import path
from .views import DepositRequestView

from . import views

urlpatterns = [
    path("api/deposit/", views.deposit_view),
    path("api/withdraw/", views.withdraw_view),
    path("api/create-account/", views.create_account_view),
    path("api/account/<str:account_id>/balance/", views.account_balance_view),
    path("api/accounts/", views.accounts_view),
    path("api/transactions/", views.transactions_view),
    path("api/account/<str:account_id>/transactions/", views.account_transactions_view),
]
