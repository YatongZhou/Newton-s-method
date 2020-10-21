/********************************************************************************
 *  newton_irr_calc
 *  根据分期本金、分期期数、分期手续费以及分摊参数，使用牛顿迭代法计算年化利率(含%)
 *  
 *  计算公式如下：
 *  原函数snFunc(irr0) = -(本金) + (第1期还款额/(1+irr0)^1) + (第2期还款额/(1+irr0)^2) +...
 *         +(第n期还款额/(1+irr0)^n);
 *  导数函数snDfunc(irr0) = ((-1)*第1期还款额)/(1+irr0)^2) + ((-2)*第2期还款额)/(1+irr0)^3) + ...
 *         + ((-n)*第n期还款额)/(1+irr0)^(n+1));
 *  迭代公式为：
 *  Irr1 = Irr0 - snFunc(irr0) / snDfunc(irr0);
 *  其中n为分期期数，Irr0、Irr1为内部收益率(月利率)，Irr0初始值为0;最终年化利率(%) = Irr1*12*100;
 *  
 *  当原函数snFunc(irr0)值小于1e-5或者计算Irr两次的差值(Irr1 - Irr0)小于1e-5时，结束迭代;否则继续
 *  迭代计算直到满足要求;
 *  当迭代次数超过20次或者计算结果超过100%时，年化利率值显示99.99标识计算出错;
 *  
 *  形参：
 *      输入：
 *          mp_amt         分期本金
 *          nbr_mths       分期期数
 *          fee_amt        分期手续费
 *          instl_flag     本金分摊方式
 *          fee_flag       手续费分摊方式
 *          instl_pcnt     本金分摊比例
 *          fee_mths       单独分期期数
 *          
 *
 *      输出：
 *          无
 *
 *  返回值：
 *          unTemp_pcnt-年化利率
 *          1.00 - 业务报错
 *******************************************************************************/
#include <iostream>
#include <iomanip> //要用到格式控制符
#include<cmath>
using namespace std;

double newton_irr_calc(  double       mp_amt,  /*分期本金*/
                      int          nbr_mths,  /*分期期数*/
                      int       fee_amt,  /*分期手续费*/
                      int          instl_flag,  /*本金分摊方式*/ 
                      int          fee_flag,  /*手续费分摊方式*/ 
                      int          instl_pcnt,  /*本金分摊比例*/
                      int         fee_mths  /*单独收取期数*/
                      )
{
    int               snResult = 0;
    int               unCount1 = 0; /*解方程的迭代次数*/
    int               unCount = 0; /*期数*/
    
    double            unMp_instalmt = 0.00; /*本金每期分摊金额*/
    double            unFee_instalmt = 0.00; /*手续费每期分摊金额*/
    double            unTemp_pcnt = 0.00;/*临时存储年化利率*/
    double            snArrInstlmt[361]; /*数组存储每期还款额，首位为负本金，故为：360+1*/

    double            snIrr0 = 0.00; /*内部收益率(月利率)的初始值为0*/
    double            snIrr1 = 0.00; /*内部收益率(月利率)的迭代值*/
    double            snFunc = 0.00; /*所求解函数值*/
    double            snDfunc = 0.00; /*所求解函数导数值*/

    /* 变量初始化 */
    memset(snArrInstlmt, 0x00, sizeof(snArrInstlmt));

    /*********************************输入项参数有效性检查    **************************/ 
	const float EPSINON = 0.00001;
    /*分期金额检查,不输入时取10000*/
    if ((mp_amt >= - EPSINON) && (mp_amt <= EPSINON))
    {
         mp_amt = 10000.00;
    }
    else if (mp_amt < 0.00)
    {
		cout<<"mp_amt error"<<mp_amt<<endl;
        return 1.00;
    }

    /*分期手续费金额检查*/
    if (fee_amt < 0.00 )
    {
        cout<<"fee_amt error"<<fee_amt<<endl;
        return 1.00;
    }

    /*分期期数检查*/
    if (( nbr_mths <= 0) || ( nbr_mths > 360))
    {
        cout<<"nbr_mths error"<<nbr_mths<<endl;

        return 1.00;
    }

    /*本金分摊方式检查*/
    if (( instl_flag != 0) && ( instl_flag != 1))
    {
        cout<<"instl_flag error"<<instl_flag<<endl;
        return 1.00;
    }

    /*手续费分摊方式检查*/
    if (( fee_flag != 0) && ( fee_flag != 1) && ( fee_flag != 2))
    {
        cout<<"fee_flag error"<<fee_flag<<endl;
        return 1.00;
    }

    /*本金分摊比例检查*/
    if ((( instl_pcnt < 0) || ( instl_pcnt >= 100)) && (1 ==  instl_flag))
    {
        cout<<"instl_pcnt error"<<instl_pcnt<<endl;
        return 1.00;
    }

    /*单独收取期数检查*/
    if ((( fee_mths <= 0) || ( fee_mths >  nbr_mths)) && (2 ==  fee_flag))
    {
        cout<<"fee_mths error"<<fee_mths<<endl;
        return 1.00;
    }


    /*****************本金、手续费不同分摊方式下，每期还款额数组赋值********************/ 

    snArrInstlmt[0] = CmdROU((-1) *  mp_amt, 2);
    /*本金平均分摊*/
    if (0 ==  instl_flag)
    {
        unMp_instalmt = (int)(mp_amt /  nbr_mths)*100+0.5)/100.0;
    }
    /*本金按比例分摊*/
    else if (1 ==  instl_flag)
    {
        unMp_instalmt = (int)((mp_amt /  nbr_mths *  instl_pcnt * 0.01, 2)*100+0.5)/100.0;
 
    }

    for (unCount = 1; unCount <  nbr_mths; unCount++)
    {
        snArrInstlmt[unCount] += unMp_instalmt; 
    }
    snArrInstlmt[unCount] +=  mp_amt - unMp_instalmt * ( nbr_mths - 1);

    /*手续费一次性收取*/
    if (0 ==  fee_flag)
    {
        snArrInstlmt[1] +=  fee_amt;
    }
    /*手续费平均分摊*/
    else if (1 ==  fee_flag)
    {
        unFee_instalmt = CmdROU( fee_amt /  nbr_mths, 2);
        for (unCount = 1; unCount <  nbr_mths; unCount++)
        {
            snArrInstlmt[unCount] += unFee_instalmt; 
        }
        snArrInstlmt[unCount] +=  fee_amt - unFee_instalmt * ( nbr_mths - 1);
    }
    /*手续费单独期数分摊*/
    else if (2 ==  fee_flag)
    {

        unFee_instalmt = CmdROU( fee_amt /  fee_mths, 2);
        for (unCount = 1; unCount <  fee_mths; unCount++)
        {
            snArrInstlmt[unCount] += unFee_instalmt; 
        }
        snArrInstlmt[unCount] +=  fee_amt - unFee_instalmt * ( fee_mths - 1);
    }
    
    /*******************牛顿迭代法计算年化利率***********************************/ 
    for (unCount1 = 0; unCount1 < 20; unCount1++)
    {
        snFunc = snArrInstlmt[0];
        snDfunc = 0.00;

        for (unCount = 1; unCount <=  nbr_mths; unCount++)
        {
            snFunc += snArrInstlmt[unCount] / pow(1 + snIrr0, unCount); 
            snDfunc -= unCount * snArrInstlmt[unCount] / pow(1 + snIrr0, unCount + 1);  
        }

        if (CmdDblCmp(snDfunc, 0.00, 6) != 0)
        {
            snIrr1 = snIrr0 - snFunc / snDfunc;
        }
        else
        {
            cout<<"snFunc error"<<snFunc<<endl;
            return 1;
        }

        /*当snFunc的值小于1e-5或者irr相邻两次的差值小于1e-5时结束循环*/
        if ((fabs(snFunc) < 1e-5) || (fabs(snIrr1 - snIrr0) < 1e-5))
        {
            break;
        }
        else
        {
            snIrr0 = snIrr1;
        }       
    }

    /*迭代计算次数超过限制*/
    if (unCount1 >= 20)
    {    
	     cout<<"Intertions error"<<unCount1<<endl;
         return 99.99;	

    }

    /*计算年化利率=irr*12*100，年化利率不能超过100%*/
    unTemp_pcnt = (int)(snIrr1 * 12 *100)*100+0.5)/100.0;
    
    /*防止出现-0.00的结果*/
    if (0.00 == unTemp_pcnt)
    {
        
        return 0.00;
    }
    
    if (unTemp_pcnt >= 100.00)
    {
        return 99.99;	
    }

    /*返回年化利率的四舍五入值*/
    return unTemp_pcnt;
          
}
/*测试程序*/
int main(int argc, char**argv)
{
    double       mp_amt;  /*分期本金*/
    int          nbr_mths;  /*分期期数*/
    int          fee_amt,;  /*分期手续费*/
    int          instl_flag;  /*本金分摊方式*/ 
    int          fee_flag;  /*手续费分摊方式*/ 
    int          instl_pcnt;  /*本金分摊比例*/
    int          fee_mths;  /*单独收取期数*/
	
	double       mp_irr;/*年化利率*/
	
	cout<<"请输入分期本金"<<endl;
	cin>>mp_amt;
	cout<<"请输入分期期数"<<endl;
	cin>>nbr_mths;
	cout<<"请输入分期手续费"<<endl;
	cin>>fee_amt;
	cout<<"请输入本金分摊方式"<<endl;
	cin>>instl_flag;
	cout<<"请输入手续费分摊方式"<<endl;
	cin>>fee_flag;
	cout<<"请输入本金分摊比例"<<endl;
	cin>>instl_pcnt;
	cout<<"请输入单独收取期数"<<endl;
	cin>>fee_mths;
	
	mp_irr = newton_irr_calc(mp_amt, nbr_mths, \
	                         nbr_mths, fee_amt, \
							 instl_flag, fee_flag, \
							 instl_pcnt, fee_mths);
							 
	cout<<"计算出的年化利率为:<<mp_irr<<endl;
	return 0;
}
