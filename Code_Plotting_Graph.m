%Graph for profit & number of transaction
[hAxes,hBar,hLine]=plotyy(VarName1, dp_al, VarName1, tv_al,'bar','plot');
legend('Daily Profit','Num of Trans');
[hAxes,hBar,hLine]=plotyy(VarName1, dp_cu, VarName1, tv_cu,'bar','plot');
legend('Daily Profit','Num of Trans');
[hAxes,hBar,hLine]=plotyy(VarName1, dp_ni, VarName1, tv_ni,'bar','plot');
legend('Daily Profit','Num of Trans');
[hAxes,hBar,hLine]=plotyy(VarName1, dp_rb, VarName1, tv_rb,'bar','plot');
legend('Daily Profit','Num of Trans');
[hAxes,hBar,hLine]=plotyy(VarName1, dp_ru, VarName1, tv_ru,'bar','plot');
legend('Daily Profit','Num of Trans');
%Graph for profit & price volatility
[hAxes,hBar,hLine]=plotyy(VarName1, dp_al, VarName1, vol_al,'bar','plot');
legend('Daily Profit','Volatility');
[hAxes,hBar,hLine]=plotyy(VarName1, dp_cu, VarName1, vol_cu,'bar','plot');
legend('Daily Profit','Volatility');
[hAxes,hBar,hLine]=plotyy(VarName1, dp_ni, VarName1, vol_ni,'bar','plot');
legend('Daily Profit','Volatility');
[hAxes,hBar,hLine]=plotyy(VarName1, dp_rb, VarName1, vol_rb,'bar','plot');
legend('Daily Profit','Volatility');
[hAxes,hBar,hLine]=plotyy(VarName1, dp_ru, VarName1, vol_ru,'bar','plot');
legend('Daily Profit','Volatility');
