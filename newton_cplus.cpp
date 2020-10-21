/********************************************************************************
 *  newton_irr_calc
 *  ���ݷ��ڱ��𡢷��������������������Լ���̯������ʹ��ţ�ٵ����������껯����(��%)
 *  
 *  ���㹫ʽ���£�
 *  ԭ����snFunc(irr0) = -(����) + (��1�ڻ����/(1+irr0)^1) + (��2�ڻ����/(1+irr0)^2) +...
 *         +(��n�ڻ����/(1+irr0)^n);
 *  ��������snDfunc(irr0) = ((-1)*��1�ڻ����)/(1+irr0)^2) + ((-2)*��2�ڻ����)/(1+irr0)^3) + ...
 *         + ((-n)*��n�ڻ����)/(1+irr0)^(n+1));
 *  ������ʽΪ��
 *  Irr1 = Irr0 - snFunc(irr0) / snDfunc(irr0);
 *  ����nΪ����������Irr0��Irr1Ϊ�ڲ�������(������)��Irr0��ʼֵΪ0;�����껯����(%) = Irr1*12*100;
 *  
 *  ��ԭ����snFunc(irr0)ֵС��1e-5���߼���Irr���εĲ�ֵ(Irr1 - Irr0)С��1e-5ʱ����������;�������
 *  ��������ֱ������Ҫ��;
 *  ��������������20�λ��߼���������100%ʱ���껯����ֵ��ʾ99.99��ʶ�������;
 *  
 *  �βΣ�
 *      ���룺
 *          mp_amt         ���ڱ���
 *          nbr_mths       ��������
 *          fee_amt        ����������
 *          instl_flag     �����̯��ʽ
 *          fee_flag       �����ѷ�̯��ʽ
 *          instl_pcnt     �����̯����
 *          fee_mths       ������������
 *          
 *
 *      �����
 *          ��
 *
 *  ����ֵ��
 *          unTemp_pcnt-�껯����
 *          1.00 - ҵ�񱨴�
 *******************************************************************************/
#include <iostream>
#include <iomanip> //Ҫ�õ���ʽ���Ʒ�
#include<cmath>
using namespace std;

double newton_irr_calc(  double       mp_amt,  /*���ڱ���*/
                      int          nbr_mths,  /*��������*/
                      int       fee_amt,  /*����������*/
                      int          instl_flag,  /*�����̯��ʽ*/ 
                      int          fee_flag,  /*�����ѷ�̯��ʽ*/ 
                      int          instl_pcnt,  /*�����̯����*/
                      int         fee_mths  /*������ȡ����*/
                      )
{
    int               snResult = 0;
    int               unCount1 = 0; /*�ⷽ�̵ĵ�������*/
    int               unCount = 0; /*����*/
    
    double            unMp_instalmt = 0.00; /*����ÿ�ڷ�̯���*/
    double            unFee_instalmt = 0.00; /*������ÿ�ڷ�̯���*/
    double            unTemp_pcnt = 0.00;/*��ʱ�洢�껯����*/
    double            snArrInstlmt[361]; /*����洢ÿ�ڻ�����λΪ�����𣬹�Ϊ��360+1*/

    double            snIrr0 = 0.00; /*�ڲ�������(������)�ĳ�ʼֵΪ0*/
    double            snIrr1 = 0.00; /*�ڲ�������(������)�ĵ���ֵ*/
    double            snFunc = 0.00; /*����⺯��ֵ*/
    double            snDfunc = 0.00; /*����⺯������ֵ*/

    /* ������ʼ�� */
    memset(snArrInstlmt, 0x00, sizeof(snArrInstlmt));

    /*********************************�����������Ч�Լ��    **************************/ 
	const float EPSINON = 0.00001;
    /*���ڽ����,������ʱȡ10000*/
    if ((mp_amt >= - EPSINON) && (mp_amt <= EPSINON))
    {
         mp_amt = 10000.00;
    }
    else if (mp_amt < 0.00)
    {
		cout<<"mp_amt error"<<mp_amt<<endl;
        return 1.00;
    }

    /*���������ѽ����*/
    if (fee_amt < 0.00 )
    {
        cout<<"fee_amt error"<<fee_amt<<endl;
        return 1.00;
    }

    /*�����������*/
    if (( nbr_mths <= 0) || ( nbr_mths > 360))
    {
        cout<<"nbr_mths error"<<nbr_mths<<endl;

        return 1.00;
    }

    /*�����̯��ʽ���*/
    if (( instl_flag != 0) && ( instl_flag != 1))
    {
        cout<<"instl_flag error"<<instl_flag<<endl;
        return 1.00;
    }

    /*�����ѷ�̯��ʽ���*/
    if (( fee_flag != 0) && ( fee_flag != 1) && ( fee_flag != 2))
    {
        cout<<"fee_flag error"<<fee_flag<<endl;
        return 1.00;
    }

    /*�����̯�������*/
    if ((( instl_pcnt < 0) || ( instl_pcnt >= 100)) && (1 ==  instl_flag))
    {
        cout<<"instl_pcnt error"<<instl_pcnt<<endl;
        return 1.00;
    }

    /*������ȡ�������*/
    if ((( fee_mths <= 0) || ( fee_mths >  nbr_mths)) && (2 ==  fee_flag))
    {
        cout<<"fee_mths error"<<fee_mths<<endl;
        return 1.00;
    }


    /*****************���������Ѳ�ͬ��̯��ʽ�£�ÿ�ڻ�������鸳ֵ********************/ 

    snArrInstlmt[0] = CmdROU((-1) *  mp_amt, 2);
    /*����ƽ����̯*/
    if (0 ==  instl_flag)
    {
        unMp_instalmt = (int)(mp_amt /  nbr_mths)*100+0.5)/100.0;
    }
    /*���𰴱�����̯*/
    else if (1 ==  instl_flag)
    {
        unMp_instalmt = (int)((mp_amt /  nbr_mths *  instl_pcnt * 0.01, 2)*100+0.5)/100.0;
 
    }

    for (unCount = 1; unCount <  nbr_mths; unCount++)
    {
        snArrInstlmt[unCount] += unMp_instalmt; 
    }
    snArrInstlmt[unCount] +=  mp_amt - unMp_instalmt * ( nbr_mths - 1);

    /*������һ������ȡ*/
    if (0 ==  fee_flag)
    {
        snArrInstlmt[1] +=  fee_amt;
    }
    /*������ƽ����̯*/
    else if (1 ==  fee_flag)
    {
        unFee_instalmt = CmdROU( fee_amt /  nbr_mths, 2);
        for (unCount = 1; unCount <  nbr_mths; unCount++)
        {
            snArrInstlmt[unCount] += unFee_instalmt; 
        }
        snArrInstlmt[unCount] +=  fee_amt - unFee_instalmt * ( nbr_mths - 1);
    }
    /*�����ѵ���������̯*/
    else if (2 ==  fee_flag)
    {

        unFee_instalmt = CmdROU( fee_amt /  fee_mths, 2);
        for (unCount = 1; unCount <  fee_mths; unCount++)
        {
            snArrInstlmt[unCount] += unFee_instalmt; 
        }
        snArrInstlmt[unCount] +=  fee_amt - unFee_instalmt * ( fee_mths - 1);
    }
    
    /*******************ţ�ٵ����������껯����***********************************/ 
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

        /*��snFunc��ֵС��1e-5����irr�������εĲ�ֵС��1e-5ʱ����ѭ��*/
        if ((fabs(snFunc) < 1e-5) || (fabs(snIrr1 - snIrr0) < 1e-5))
        {
            break;
        }
        else
        {
            snIrr0 = snIrr1;
        }       
    }

    /*�������������������*/
    if (unCount1 >= 20)
    {    
	     cout<<"Intertions error"<<unCount1<<endl;
         return 99.99;	

    }

    /*�����껯����=irr*12*100���껯���ʲ��ܳ���100%*/
    unTemp_pcnt = (int)(snIrr1 * 12 *100)*100+0.5)/100.0;
    
    /*��ֹ����-0.00�Ľ��*/
    if (0.00 == unTemp_pcnt)
    {
        
        return 0.00;
    }
    
    if (unTemp_pcnt >= 100.00)
    {
        return 99.99;	
    }

    /*�����껯���ʵ���������ֵ*/
    return unTemp_pcnt;
          
}
/*���Գ���*/
int main(int argc, char**argv)
{
    double       mp_amt;  /*���ڱ���*/
    int          nbr_mths;  /*��������*/
    int          fee_amt,;  /*����������*/
    int          instl_flag;  /*�����̯��ʽ*/ 
    int          fee_flag;  /*�����ѷ�̯��ʽ*/ 
    int          instl_pcnt;  /*�����̯����*/
    int          fee_mths;  /*������ȡ����*/
	
	double       mp_irr;/*�껯����*/
	
	cout<<"��������ڱ���"<<endl;
	cin>>mp_amt;
	cout<<"�������������"<<endl;
	cin>>nbr_mths;
	cout<<"���������������"<<endl;
	cin>>fee_amt;
	cout<<"�����뱾���̯��ʽ"<<endl;
	cin>>instl_flag;
	cout<<"�����������ѷ�̯��ʽ"<<endl;
	cin>>fee_flag;
	cout<<"�����뱾���̯����"<<endl;
	cin>>instl_pcnt;
	cout<<"�����뵥����ȡ����"<<endl;
	cin>>fee_mths;
	
	mp_irr = newton_irr_calc(mp_amt, nbr_mths, \
	                         nbr_mths, fee_amt, \
							 instl_flag, fee_flag, \
							 instl_pcnt, fee_mths);
							 
	cout<<"��������껯����Ϊ:<<mp_irr<<endl;
	return 0;
}
