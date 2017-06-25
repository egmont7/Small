#include<cstdio>
#include<iostream>
using namespace std;

#define TOTAL 20000

int main(int, char**){
    int coins[] = {1, 2, 5, 10, 20, 50, 100, 200};
    long N[TOTAL+1][8];
    for (int i=0; i<8; i++) N[0][i] = 1;
    for (int i=0; i<TOTAL+1; i++) N[i][0] = 1;

    for (int coin_idx=1; coin_idx<8; coin_idx++){
        for(int val=1; val<=TOTAL; val++){
            N[val][coin_idx] = 0;
            for(int i=0; i<=val/coins[coin_idx]; i++){
                N[val][coin_idx] += N[val-coins[coin_idx]*i][coin_idx-1];
            }
        }
    }
    /*
    for (int i=TOTAL; i>=0; i--){
        printf("% 3d: % 8d % 8d % 8d % 8d % 8d % 8d % 8d % 8d\n",
                i, N[i][0],N[i][1],N[i][2],N[i][3],N[i][4],N[i][5],N[i][6],N[i][7]);
    }
    */
    /* printf("%dl\n", N[TOTAL][7]); */
    cout << N[TOTAL][7] << endl;
}
