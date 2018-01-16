module Main (main) -- solid
where {
   import System.Environment (getArgs);
--partain: import Fast2haskell;
#include "../Fast2haskell.hs"
    strict_show_i::Int -> [Char];
    strict_show_i x=miraseq x (show x);
    strict_show_d::Double -> [Char];
    strict_show_d x=miraseq x (show x);

    f_benchmark_main a_n=f_concat [(++) (f_f_main_test a_x) "\n"|a_x<-[(1 :: Int)..a_n]];
type 
    T_t_std_real=Double;
    f_f_std_error ((a_x,a_s):a_y)=abortstr a_s;
    f_f_std_index a_lst a_n=(!!) a_lst a_n;
    f_id_THEPRIME a_x=a_x;
    f_eq_c a_x a_y=((==) :: (Int -> Int -> Bool)) (fromEnum a_x) (fromEnum a_y);
    f_f_StandardXfer_strtoreal a_i=f_stor a_i;
    f_f_StandardXfer_strtonum a_i=f_ston a_i;
    f_f_StandardXfer_realtostr a_r=strict_show_d a_r;
    f_f_StandardXfer_numtostr a_r=strict_show_i a_r;
    f_f_StandardXfer_skipwhitespace (' ':a_inp)=f_f_StandardXfer_skipwhitespace a_inp;
    f_f_StandardXfer_skipwhitespace ('\o011':a_inp)=f_f_StandardXfer_skipwhitespace a_inp;
    f_f_StandardXfer_skipwhitespace ('\o012':a_inp)=f_f_StandardXfer_skipwhitespace a_inp;
    f_f_StandardXfer_skipwhitespace a_x=a_x;
    f_splitwhile::(Char -> Bool) -> [Char] -> ([Char],[Char]);
    f_splitwhile a_pred []=([],[]);
    f_splitwhile a_pred (a_a:a_b)=
        let { 
            (r_rest,r_l2)=f_splitwhile a_pred a_b;
            r_l1=(:) a_a r_rest
         } in  
            if (a_pred a_a)
            then (r_l1,r_l2)
            else 
                ([],(:) a_a a_b);
    f_ston::[Char] -> (Int,[Char]);
    f_ston a_list=
        let { 
            (r_number,r_rest)=f_splitwhile f_digit a_list;
            r_i=f_atoi r_number
         } in  (r_i,r_rest);
    f_stor::[Char] -> (Double,[Char]);
    f_stor a_list=
        let { 
            (r_number,r_rest)=f_splitwhile f_digitordot a_list;
            r_i=f_atof r_number
         } in  (r_i,r_rest);
    f_digitordot a_x=
        if (f_digit a_x)
        then True
        else 
            (f_eq_c a_x '.');
    f_atoi a_n=
        let { 
            f_comb a_res a_number=((-) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) (((*) :: (Int -> Int -> Int)) (10 :: Int) a_res) (fromEnum a_number)) (fromEnum '0')
         } in  f_foldl f_comb (0 :: Int) a_n;
    f_atof a_n=
        let { 
            (r_l1,r_dot_l2)=f_splitwhile f_digit a_n;
            r_l2=tail r_dot_l2;
            r_x=fromIntegral (f_atoi r_l1);
            r_y=fromIntegral (f_atoi r_l2)
         } in  ((+) :: (Double -> Double -> Double)) r_x (((/) :: (Double -> Double -> Double)) r_y ((**) (10.0000 :: Double) (fromIntegral (length r_l2))));
    f_f_StandardEnviron_fromfile a_x=c_aap;
    f_f_StandardEnviron_getarg a_x="x";
    f_f_StandardEnviron_termout a_zx=a_zx;
    c_aap=(++) "599\n" ((++) "hhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhu" ((++) "huhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhu" ((++) "huhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuh" ((++) "uhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhuhu\n" ((++) "300\n" 
        ((++) "ci   0.021070        0.968071        0.066731        0       \n" ((++) "ci   0.128374        0.512205        0.202019        1       \n" ((++) "ci   0.511382        0.703298        0.737414        2       \n" ((++) "ci   0.299550        0.762057        0.456381        3       \n" ((++) "ci   0.041452        0.612559        0.073381        4       \n" ((++) "ci   0.142607        0.564916        0.697869        5       \n" ((++) "ci   0.346689        0.807441        0.877656        6       \n" 
        ((++) "ci   0.027592        0.186270        0.670770        7       \n" ((++) "ci   0.071955        0.763264        0.156725        8       \n" ((++) "ci   0.140896        0.276388        0.327842        9       \n" ((++) "ci   0.058352        0.820830        0.125578        10      \n" ((++) "ci   0.418691        0.813269        0.607102        11      \n" ((++) "ci   0.226132        0.544781        0.831362        12      \n" ((++) "ci   0.093206        0.422436        0.178051        13      \n" 
        ((++) "ci   0.120798        0.812941        0.865829        14      \n" ((++) "ci   0.010864        0.576205        0.022554        15      \n" ((++) "ci   0.188229        0.213218        0.700614        16      \n" ((++) "ci   0.287990        0.594304        0.727312        17      \n" ((++) "ci   0.032154        0.056388        0.470671        18      \n" ((++) "ci   0.147190        0.712824        0.839178        19      \n" ((++) "ci   0.171081        0.852355        0.227683        20      \n" 
        ((++) "ci   0.016953        0.432277        0.440519        21      \n" ((++) "ci   0.750842        0.896684        0.969284        22      \n" ((++) "ci   0.673985        0.776995        0.838192        23      \n" ((++) "ci   0.100658        0.243962        0.668606        24      \n" ((++) "ci   0.113814        0.700760        0.142679        25      \n" ((++) "ci   0.225801        0.435030        0.313384        26      \n" ((++) "ci   0.022801        0.065182        0.234326        27      \n" 
        ((++) "ci   0.001502        0.002708        0.879832        28      \n" ((++) "ci   0.119303        0.636800        0.189629        29      \n" ((++) "ci   0.668399        0.999236        0.912907        30      \n" ((++) "ci   0.074608        0.677019        0.084698        31      \n" ((++) "ci   0.264237        0.474898        0.725914        32      \n" ((++) "ci   0.177144        0.594201        0.725150        33      \n" ((++) "ci   0.261842        0.262600        0.402168        34      \n" 
        ((++) "ci   0.019759        0.122068        0.062188        35      \n" ((++) "ci   0.132650        0.768584        0.157506        36      \n" ((++) "ci   0.001987        0.004624        0.516635        37      \n" ((++) "ci   0.030298        0.061105        0.752307        38      \n" ((++) "ci   0.516275        0.531174        0.533306        39      \n" ((++) "ci   0.210762        0.458455        0.833743        40      \n" ((++) "ci   0.125969        0.428606        0.283307        41      \n" 
        ((++) "ci   0.016031        0.820637        0.086674        42      \n" ((++) "ci   0.144788        0.892563        0.147641        43      \n" ((++) "ci   0.303750        0.339872        0.343493        44      \n" ((++) "ci   0.119706        0.280670        0.373792        45      \n" ((++) "ci   0.479475        0.481504        0.645733        46      \n" ((++) "ci   0.006091        0.213362        0.150721        47      \n" ((++) "ci   0.490408        0.808735        0.622639        48      \n" 
        ((++) "ci   0.638049        0.842759        0.787973        49      \n" ((++) "ci   0.200045        0.768632        0.672545        50      \n" ((++) "ci   0.121083        0.197618        0.817333        51      \n" ((++) "ci   0.634564        0.820587        0.914904        52      \n" ((++) "ci   0.003262        0.034610        0.437512        53      \n" ((++) "ci   0.203770        0.361011        0.371997        54      \n" ((++) "ci   0.070113        0.159970        0.131718        55      \n" 
        ((++) "ci   0.022732        0.034753        0.595814        56      \n" ((++) "ci   0.023540        0.296602        0.120175        57      \n" ((++) "ci   0.187989        0.447007        0.309413        58      \n" ((++) "ci   0.050730        0.054135        0.640650        59      \n" ((++) "ci   0.143475        0.369828        0.850468        60      \n" ((++) "ci   0.367938        0.539006        0.893777        61      \n" ((++) "ci   0.295271        0.680585        0.769600        62      \n" 
        ((++) "ci   0.012838        0.352071        0.014918        63      \n" ((++) "ci   0.007027        0.222000        0.128909        64      \n" ((++) "ci   0.109498        0.394065        0.206391        65      \n" ((++) "ci   0.152620        0.399666        0.236447        66      \n" ((++) "ci   0.174336        0.742904        0.176265        67      \n" ((++) "ci   0.015133        0.222492        0.910964        68      \n" ((++) "ci   0.191398        0.830518        0.571453        69      \n" 
        ((++) "ci   0.097431        0.349670        0.223443        70      \n" ((++) "ci   0.064177        0.787564        0.715049        71      \n" ((++) "ci   0.111674        0.114931        0.849041        72      \n" ((++) "ci   0.126807        0.646658        0.772490        73      \n" ((++) "ci   0.318205        0.477176        0.343943        74      \n" ((++) "ci   0.530794        0.826846        0.567386        75      \n" ((++) "ci   0.393721        0.423555        0.544680        76      \n" 
        ((++) "ci   0.102595        0.580494        0.859381        77      \n" ((++) "ci   0.328618        0.959354        0.377468        78      \n" ((++) "ci   0.132480        0.251920        0.799492        79      \n" ((++) "ci   0.244154        0.697736        0.710918        80      \n" ((++) "ci   0.059554        0.344394        0.483408        81      \n" ((++) "ci   0.604234        0.963190        0.780477        82      \n" ((++) "ci   0.138656        0.558161        0.318963        83      \n" 
        ((++) "ci   0.098010        0.178345        0.100549        84      \n" ((++) "ci   0.349930        0.555813        0.429167        85      \n" ((++) "ci   0.116907        0.355305        0.158143        86      \n" ((++) "ci   0.200273        0.487785        0.855879        87      \n" ((++) "ci   0.021842        0.311233        0.102899        88      \n" ((++) "ci   0.027179        0.138584        0.792063        89      \n" ((++) "ci   0.126620        0.592875        0.187648        90      \n" 
        ((++) "ci   0.050258        0.195063        0.884466        91      \n" ((++) "ci   0.245321        0.488684        0.592631        92      \n" ((++) "ci   0.355625        0.373150        0.897595        93      \n" ((++) "ci   0.148636        0.758372        0.265713        94      \n" ((++) "ci   0.114127        0.137342        0.678321        95      \n" ((++) "ci   0.001119        0.826514        0.002849        96      \n" ((++) "ci   0.061299        0.267207        0.312650        97      \n" 
        ((++) "ci   0.201329        0.920447        0.668598        98      \n" ((++) "ci   0.041537        0.312109        0.502473        99      \n" ((++) "ci   0.055737        0.184262        0.909440        100     \n" ((++) "ci   0.047663        0.855610        0.099824        101     \n" ((++) "ci   0.028370        0.791694        0.535614        102     \n" ((++) "ci   0.078779        0.169339        0.778480        103     \n" ((++) "ci   0.016823        0.363111        0.886472        104     \n" 
        ((++) "ci   0.160506        0.320521        0.596108        105     \n" ((++) "ci   0.303168        0.865019        0.487978        106     \n" ((++) "ci   0.010788        0.820577        0.018395        107     \n" ((++) "ci   0.109996        0.623215        0.734575        108     \n" ((++) "ci   0.069733        0.757895        0.412492        109     \n" ((++) "ci   0.014058        0.228627        0.319559        110     \n" ((++) "ci   0.370734        0.389759        0.957260        111     \n" 
        ((++) "ci   0.013635        0.076998        0.014379        112     \n" ((++) "ci   0.040221        0.361455        0.043180        113     \n" ((++) "ci   0.022055        0.960949        0.225924        114     \n" ((++) "ci   0.259073        0.315279        0.398470        115     \n" ((++) "ci   0.062097        0.940022        0.097558        116     \n" ((++) "ci   0.631851        0.766385        0.953403        117     \n" ((++) "ci   0.171829        0.624430        0.491336        118     \n" 
        ((++) "ci   0.032570        0.958966        0.671115        119     \n" ((++) "ci   0.072791        0.908110        0.257700        120     \n" ((++) "ci   0.303249        0.869059        0.483624        121     \n" ((++) "ci   0.325304        0.346046        0.904548        122     \n" ((++) "ci   0.230853        0.661325        0.303019        123     \n" ((++) "ci   0.000392        0.319825        0.002424        124     \n" ((++) "ci   0.320457        0.618265        0.560755        125     \n" 
        ((++) "ci   0.207417        0.248346        0.976513        126     \n" ((++) "ci   0.022328        0.215672        0.883520        127     \n" ((++) "ci   0.226403        0.581136        0.281613        128     \n" ((++) "ci   0.477562        0.802232        0.682799        129     \n" ((++) "ci   0.001973        0.503808        0.003649        130     \n" ((++) "ci   0.103303        0.605249        0.117858        131     \n" ((++) "ci   0.016594        0.490019        0.391151        132     \n" 
        ((++) "ci   0.091869        0.936550        0.114673        133     \n" ((++) "ci   0.080134        0.356590        0.790970        134     \n" ((++) "ci   0.046729        0.075910        0.812481        135     \n" ((++) "ci   0.021154        0.022303        0.638109        136     \n" ((++) "ci   0.074259        0.537252        0.668226        137     \n" ((++) "ci   0.154857        0.309602        0.604353        138     \n" ((++) "ci   0.290172        0.720229        0.377860        139     \n" 
        ((++) "ci   0.153366        0.771277        0.706099        140     \n" ((++) "ci   0.217981        0.568157        0.252939        141     \n" ((++) "ci   0.001258        0.951805        0.853778        142     \n" ((++) "ci   0.538509        0.862247        0.874932        143     \n" ((++) "ci   0.234034        0.530473        0.428009        144     \n" ((++) "ci   0.457840        0.604732        0.737611        145     \n" ((++) "ci   0.037957        0.216247        0.740964        146     \n" 
        ((++) "ci   0.006116        0.451156        0.839065        147     \n" ((++) "ci   0.259055        0.604522        0.407222        148     \n" ((++) "ci   0.210859        0.858428        0.625203        149     \n" ((++) "ci   0.073106        0.712206        0.626461        150     \n" ((++) "ci   0.015148        0.587138        0.164970        151     \n" ((++) "ci   0.123623        0.399004        0.208311        152     \n" ((++) "ci   0.137683        0.298618        0.895326        153     \n" 
        ((++) "ci   0.160496        0.901442        0.179301        154     \n" ((++) "ci   0.029286        0.037730        0.170108        155     \n" ((++) "ci   0.179777        0.289992        0.327374        156     \n" ((++) "ci   0.385142        0.487046        0.935767        157     \n" ((++) "ci   0.496777        0.698291        0.934251        158     \n" ((++) "ci   0.105153        0.339674        0.224506        159     \n" ((++) "ci   0.713873        0.906288        0.858504        160     \n" 
        ((++) "ci   0.000610        0.000932        0.490031        161     \n" ((++) "ci   0.088206        0.409136        0.240089        162     \n" ((++) "ci   0.384466        0.565052        0.401510        163     \n" ((++) "ci   0.024383        0.567748        0.053722        164     \n" ((++) "ci   0.332574        0.578888        0.923437        165     \n" ((++) "ci   0.004625        0.024695        0.399032        166     \n" ((++) "ci   0.033392        0.952878        0.335514        167     \n" 
        ((++) "ci   0.109147        0.294928        0.210713        168     \n" ((++) "ci   0.051888        0.447727        0.199572        169     \n" ((++) "ci   0.070308        0.162413        0.337904        170     \n" ((++) "ci   0.074933        0.894377        0.725429        171     \n" ((++) "ci   0.013413        0.847255        0.060942        172     \n" ((++) "ci   0.174164        0.953253        0.348037        173     \n" ((++) "ci   0.543854        0.816601        0.696599        174     \n" 
        ((++) "ci   0.054870        0.960535        0.808103        175     \n" ((++) "ci   0.017458        0.019600        0.647507        176     \n" ((++) "ci   0.138329        0.491629        0.148005        177     \n" ((++) "ci   0.002268        0.032863        0.319332        178     \n" ((++) "ci   0.367022        0.518564        0.816587        179     \n" ((++) "ci   0.018175        0.779869        0.039821        180     \n" ((++) "ci   0.012555        0.951949        0.133277        181     \n" 
        ((++) "ci   0.392494        0.999925        0.761114        182     \n" ((++) "ci   0.432008        0.652410        0.953422        183     \n" ((++) "ci   0.299976        0.843149        0.637931        184     \n" ((++) "ci   0.044045        0.202417        0.760534        185     \n" ((++) "ci   0.083867        0.088002        0.356990        186     \n" ((++) "ci   0.030115        0.340682        0.205471        187     \n" ((++) "ci   0.003666        0.006540        0.453587        188     \n" 
        ((++) "ci   0.042494        0.351332        0.689477        189     \n" ((++) "ci   0.029414        0.360152        0.226320        190     \n" ((++) "ci   0.001476        0.131872        0.307744        191     \n" ((++) "ci   0.015180        0.387107        0.018584        192     \n" ((++) "ci   0.011591        0.163322        0.090941        193     \n" ((++) "ci   0.441295        0.562106        0.909243        194     \n" ((++) "ci   0.247153        0.403879        0.672739        195     \n" 
        ((++) "ci   0.071594        0.899991        0.113676        196     \n" ((++) "ci   0.164131        0.677669        0.251705        197     \n" ((++) "ci   0.120417        0.249725        0.186612        198     \n" ((++) "ci   0.132008        0.911253        0.728150        199     \n" ((++) "ci   0.041272        0.473359        0.637393        200     \n" ((++) "ci   0.027209        0.423656        0.161807        201     \n" ((++) "ci   0.330420        0.506598        0.807788        202     \n" 
        ((++) "ci   0.169421        0.173454        0.341672        203     \n" ((++) "ci   0.178900        0.702443        0.207226        204     \n" ((++) "ci   0.210567        0.327643        0.342239        205     \n" ((++) "ci   0.526978        0.730252        0.585715        206     \n" ((++) "ci   0.046111        0.171555        0.125460        207     \n" ((++) "ci   0.373754        0.903323        0.776343        208     \n" ((++) "ci   0.061160        0.245562        0.683393        209     \n" 
        ((++) "ci   0.024237        0.227836        0.043972        210     \n" ((++) "ci   0.397991        0.603405        0.714437        211     \n" ((++) "ci   0.231395        0.848967        0.397830        212     \n" ((++) "ci   0.275367        0.910127        0.625666        213     \n" ((++) "ci   0.385011        0.701540        0.439216        214     \n" ((++) "ci   0.193942        0.779745        0.803313        215     \n" ((++) "ci   0.218521        0.266185        0.613655        216     \n" 
        ((++) "ci   0.143352        0.337757        0.547845        217     \n" ((++) "ci   0.371098        0.783406        0.687086        218     \n" ((++) "ci   0.071181        0.124055        0.665212        219     \n" ((++) "ci   0.019043        0.062149        0.689449        220     \n" ((++) "ci   0.285227        0.351078        0.714172        221     \n" ((++) "ci   0.503749        0.964733        0.619989        222     \n" ((++) "ci   0.085896        0.302490        0.167835        223     \n" 
        ((++) "ci   0.007956        0.854921        0.018199        224     \n" ((++) "ci   0.113182        0.601995        0.483037        225     \n" ((++) "ci   0.193459        0.620920        0.275375        226     \n" ((++) "ci   0.266341        0.292826        0.818105        227     \n" ((++) "ci   0.455713        0.540131        0.646120        228     \n" ((++) "ci   0.322712        0.638313        0.408639        229     \n" ((++) "ci   0.351151        0.625431        0.814049        230     \n" 
        ((++) "ci   0.244588        0.673143        0.527014        231     \n" ((++) "ci   0.026518        0.072803        0.751143        232     \n" ((++) "ci   0.051636        0.715703        0.188323        233     \n" ((++) "ci   0.075026        0.242495        0.216575        234     \n" ((++) "ci   0.103756        0.859168        0.168416        235     \n" ((++) "ci   0.177229        0.241219        0.666656        236     \n" ((++) "ci   0.001081        0.123265        0.031758        237     \n" 
        ((++) "ci   0.284227        0.292889        0.754427        238     \n" ((++) "ci   0.056522        0.135814        0.648461        239     \n" ((++) "ci   0.106296        0.691471        0.430714        240     \n" ((++) "ci   0.096169        0.381384        0.666678        241     \n" ((++) "ci   0.218835        0.604984        0.700104        242     \n" ((++) "ci   0.373258        0.762320        0.950201        243     \n" ((++) "ci   0.190849        0.668969        0.705344        244     \n" 
        ((++) "ci   0.569357        0.715120        0.828776        245     \n" ((++) "ci   0.585520        0.831093        0.829857        246     \n" ((++) "ci   0.185046        0.626589        0.307103        247     \n" ((++) "ci   0.041040        0.340936        0.592959        248     \n" ((++) "ci   0.185052        0.811793        0.552350        249     \n" ((++) "ci   0.114607        0.221319        0.696585        250     \n" ((++) "ci   0.070170        0.222799        0.744208        251     \n" 
        ((++) "ci   0.068928        0.149068        0.198082        252     \n" ((++) "ci   0.285677        0.733328        0.606086        253     \n" ((++) "ci   0.059771        0.597327        0.065773        254     \n" ((++) "ci   0.230904        0.391035        0.800134        255     \n" ((++) "ci   0.326135        0.340685        0.819139        256     \n" ((++) "ci   0.115903        0.122142        0.848132        257     \n" ((++) "ci   0.272197        0.429498        0.544555        258     \n" 
        ((++) "ci   0.172700        0.738539        0.331373        259     \n" ((++) "ci   0.013182        0.117751        0.267259        260     \n" ((++) "ci   0.033323        0.573360        0.928255        261     \n" ((++) "ci   0.374008        0.461717        0.868903        262     \n" ((++) "ci   0.148790        0.280856        0.697133        263     \n" ((++) "ci   0.006218        0.031698        0.233027        264     \n" ((++) "ci   0.086324        0.258522        0.712329        265     \n" 
        ((++) "ci   0.142325        0.365691        0.710680        266     \n" ((++) "ci   0.067750        0.939865        0.452734        267     \n" ((++) "ci   0.154354        0.200747        0.319793        268     \n" ((++) "ci   0.043022        0.124269        0.737095        269     \n" ((++) "ci   0.084388        0.924676        0.136776        270     \n" ((++) "ci   0.024253        0.573683        0.442953        271     \n" ((++) "ci   0.015765        0.026416        0.510703        272     \n" 
        ((++) "ci   0.009382        0.261137        0.635834        273     \n" ((++) "ci   0.194813        0.609608        0.458063        274     \n" ((++) "ci   0.014361        0.568604        0.283982        275     \n" ((++) "ci   0.362157        0.710028        0.713853        276     \n" ((++) "ci   0.020228        0.368724        0.052499        277     \n" ((++) "ci   0.145503        0.289704        0.894838        278     \n" ((++) "ci   0.213319        0.396095        0.318941        279     \n" 
        ((++) "ci   0.037228        0.219888        0.179796        280     \n" ((++) "ci   0.031719        0.368892        0.556496        281     \n" ((++) "ci   0.141796        0.177485        0.579968        282     \n" ((++) "ci   0.043767        0.643505        0.502713        283     \n" ((++) "ci   0.230335        0.482424        0.616126        284     \n" ((++) "ci   0.462711        0.897140        0.679631        285     \n" ((++) "ci   0.019207        0.934368        0.048524        286     \n" 
        ((++) "ci   0.048774        0.587482        0.080242        287     \n" ((++) "ci   0.037533        0.443916        0.150381        288     \n" ((++) "ci   0.004612        0.980843        0.028150        289     \n" ((++) "ci   0.319945        0.517812        0.877017        290     \n" ((++) "ci   0.056446        0.965001        0.497530        291     \n" ((++) "ci   0.073121        0.101508        0.494552        292     \n" ((++) "ci   0.033914        0.662602        0.068795        293     \n" 
        ((++) "ci   0.032689        0.207173        0.070636        294     \n" ((++) "ci   0.080930        0.145858        0.994112        295     \n" ((++) "ci   0.114844        0.386773        0.188445        296     \n" ((++) "ci   0.071973        0.593946        0.259080        297     \n" ((++) "ci   0.001445        0.769506        0.016744        298     \n" "ci   0.064137        0.835918        0.130956        299     \n"))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
    f_f_StandardLists_length a_x=length a_x;
    f_f_StandardVectors_listtovector a_x=a_x;
type 
    T_t_Misc_SCALAR=T_t_std_real;
    f_f_Misc_hd (a_f_Misc__a:a_f_Misc__l)=a_f_Misc__a;
    f_f_Misc_tl []=[];
    f_f_Misc_tl (a_f_Misc__a:a_f_Misc__l)=a_f_Misc__l;
    f_f_Misc_fs [] a_f_Misc__y1 a_f_Misc__y2 a_wC_Misc_0_=(++) a_f_Misc__y1 a_f_Misc__y2;
    f_f_Misc_fs (a_f_Misc__x:a_f_Misc__xs) a_f_Misc__y1 [] a_f_Misc__n=f_f_Misc_fs ((:) a_f_Misc__x a_f_Misc__xs) [] a_f_Misc__y1 a_f_Misc__n;
    f_f_Misc_fs (a_f_Misc__x:a_f_Misc__xs) a_f_Misc__y1 (a_f_Misc__y2:a_f_Misc__y2s) a_f_Misc__n=f_f_Misc_fs a_f_Misc__xs ((++) a_f_Misc__y1 ((:) ((++) a_f_Misc__y2 ((:) a_f_Misc__x [])) [])) a_f_Misc__y2s a_f_Misc__n;
    f_f_Misc_splitfirst [] a_f_Misc__y a_f_Misc__p a_f_Misc__n=
        let { 
            r_wC_Misc_1_=f_id_THEPRIME a_f_Misc__y;
            r_wC_Misc_2_=f_f_Misc_splitfirst [] ((++) a_f_Misc__y []) (((-) :: (Int -> Int -> Int)) a_f_Misc__p (1 :: Int)) a_f_Misc__n
         } in  
            if (((==) :: (Int -> Int -> Bool)) a_f_Misc__p (0 :: Int))
            then r_wC_Misc_1_
            else 
                r_wC_Misc_2_;
    f_f_Misc_splitfirst (a_f_Misc__x:a_f_Misc__xs) a_f_Misc__y a_f_Misc__p a_f_Misc__n=
        let { 
            r_wC_Misc_3_=f_f_Misc_fs ((:) a_f_Misc__x a_f_Misc__xs) [] a_f_Misc__y a_f_Misc__n;
            r_wC_Misc_4_=f_f_Misc_splitfirst a_f_Misc__xs ((++) a_f_Misc__y ((:) ((:) a_f_Misc__x []) [])) (((-) :: (Int -> Int -> Int)) a_f_Misc__p (1 :: Int)) a_f_Misc__n
         } in  
            if (((==) :: (Int -> Int -> Bool)) a_f_Misc__p (0 :: Int))
            then r_wC_Misc_3_
            else 
                r_wC_Misc_4_;
    f_f_Misc_fastsplit a_f_Misc__x a_f_Misc__n=f_f_Misc_splitfirst a_f_Misc__x [] a_f_Misc__n a_f_Misc__n;
    f_f_Misc_share [] 0=([],[]);
    f_f_Misc_share (a_f_Misc__x:a_f_Misc__xs) a_f_Misc__n=
        let { 
            r_wC_Misc_5_=([],(:) a_f_Misc__x a_f_Misc__xs);
            r_wC_Misc_6_=
                let { 
                    (r_f_Misc__a,r_f_Misc__b)=f_f_Misc_share a_f_Misc__xs (((-) :: (Int -> Int -> Int)) a_f_Misc__n (1 :: Int));
                    r_wC_Misc_7_=((:) a_f_Misc__x r_f_Misc__a,r_f_Misc__b)
                 } in  f_id_THEPRIME r_wC_Misc_7_
         } in  
            if (((==) :: (Int -> Int -> Bool)) a_f_Misc__n (0 :: Int))
            then r_wC_Misc_5_
            else 
                r_wC_Misc_6_;
    f_f_Misc_spl2 [] a_f_Misc__y a_wC_Misc_8_=a_f_Misc__y;
    f_f_Misc_spl2 a_f_Misc__x a_f_Misc__y a_f_Misc__s=
        let { 
            (r_f_Misc__a,r_f_Misc__b)=f_f_Misc_share a_f_Misc__x a_f_Misc__s;
            r_wC_Misc_9_=f_f_Misc_spl2 r_f_Misc__b ((++) a_f_Misc__y ((:) r_f_Misc__a [])) a_f_Misc__s
         } in  r_wC_Misc_9_;
    f_f_Misc_spl [] a_f_Misc__y a_wC_Misc_10_ a_wC_Misc_11_=a_f_Misc__y;
    f_f_Misc_spl a_f_Misc__x a_f_Misc__y a_f_Misc__s a_f_Misc__e=
        let { 
            (r_f_Misc__a,r_f_Misc__b)=f_f_Misc_share a_f_Misc__x a_f_Misc__s;
            r_wC_Misc_12_=
                let { 
                    r_wC_Misc_13_=f_f_Misc_spl r_f_Misc__b ((++) a_f_Misc__y ((:) r_f_Misc__a [])) a_f_Misc__s (((-) :: (Int -> Int -> Int)) a_f_Misc__e (1 :: Int));
                    r_wC_Misc_14_=f_f_Misc_spl2 a_f_Misc__x a_f_Misc__y (((-) :: (Int -> Int -> Int)) a_f_Misc__s (1 :: Int))
                 } in  
                    if (((>) :: (Int -> Int -> Bool)) a_f_Misc__e (0 :: Int))
                    then r_wC_Misc_13_
                    else 
                        r_wC_Misc_14_
         } in  r_wC_Misc_12_;
    f_f_Misc_split a_f_Misc__x a_f_Misc__n=
        let { 
            r_f_Misc__l=f_f_StandardLists_length a_f_Misc__x;
            r_wC_Misc_15_=
                let { 
                    r_f_Misc__e=((rem) :: (Int -> Int -> Int)) r_f_Misc__l a_f_Misc__n;
                    r_wC_Misc_16_=
                        let { 
                            r_wC_Misc_17_=f_f_Misc_spl a_f_Misc__x [] (((+) :: (Int -> Int -> Int)) (((quot) :: (Int -> Int -> Int)) r_f_Misc__l a_f_Misc__n) (1 :: Int)) r_f_Misc__e;
                            r_wC_Misc_18_=f_f_Misc_spl2 a_f_Misc__x [] (((quot) :: (Int -> Int -> Int)) r_f_Misc__l a_f_Misc__n)
                         } in  
                            if (((>) :: (Int -> Int -> Bool)) r_f_Misc__e (0 :: Int))
                            then r_wC_Misc_17_
                            else 
                                r_wC_Misc_18_
                 } in  f_id_THEPRIME r_wC_Misc_16_
         } in  r_wC_Misc_15_;
    f_f_Misc_split_1 a_f_Misc__x a_f_Misc__n a_f_Misc__l=
        let { 
            r_f_Misc__e=((rem) :: (Int -> Int -> Int)) a_f_Misc__l a_f_Misc__n;
            r_wC_Misc_19_=
                let { 
                    r_wC_Misc_20_=f_f_Misc_spl a_f_Misc__x [] (((+) :: (Int -> Int -> Int)) (((quot) :: (Int -> Int -> Int)) a_f_Misc__l a_f_Misc__n) (1 :: Int)) r_f_Misc__e;
                    r_wC_Misc_21_=f_f_Misc_spl2 a_f_Misc__x [] (((quot) :: (Int -> Int -> Int)) a_f_Misc__l a_f_Misc__n)
                 } in  
                    if (((>) :: (Int -> Int -> Bool)) r_f_Misc__e (0 :: Int))
                    then r_wC_Misc_20_
                    else 
                        r_wC_Misc_21_
         } in  r_wC_Misc_19_;
    f_f_Misc_getinteger a_f_Misc__x=f_f_StandardXfer_strtonum (f_f_StandardXfer_skipwhitespace a_f_Misc__x);
    f_f_Misc_getreal a_f_Misc__x=f_f_StandardXfer_strtoreal (f_f_StandardXfer_skipwhitespace a_f_Misc__x);
    f_f_Misc_getstring a_f_Misc__x a_f_Misc__n=
        let { 
            r_wC_Misc_22_=([],a_f_Misc__x);
            r_wC_Misc_23_=
                let { 
                    r_f_Misc__z=f_f_StandardXfer_skipwhitespace a_f_Misc__x;
                    r_wC_Misc_24_=
                        let { 
                            (r_f_Misc__restchr,r_f_Misc__leftover)=f_f_Misc_getstring (f_f_Misc_tl r_f_Misc__z) (((-) :: (Int -> Int -> Int)) a_f_Misc__n (1 :: Int));
                            r_wC_Misc_25_=((:) (f_f_Misc_hd r_f_Misc__z) r_f_Misc__restchr,r_f_Misc__leftover)
                         } in  f_id_THEPRIME r_wC_Misc_25_
                 } in  f_id_THEPRIME r_wC_Misc_24_
         } in  
            if (((==) :: (Int -> Int -> Bool)) a_f_Misc__n (0 :: Int))
            then r_wC_Misc_22_
            else 
                r_wC_Misc_23_;
    f_f_Misc_readlist_1 a_f_Misc__stream a_wC_Misc_26_ 0=([],a_f_Misc__stream);
    f_f_Misc_readlist_1 a_f_Misc__stream a_f_Misc__getitem a_f_Misc__n=
        let { 
            (r_f_Misc__p,r_f_Misc__rest)=a_f_Misc__getitem a_f_Misc__stream;
            r_wC_Misc_27_=
                let { 
                    (r_f_Misc__ps,r_f_Misc__rest2)=f_f_Misc_readlist_1 (f_f_StandardXfer_skipwhitespace r_f_Misc__rest) a_f_Misc__getitem (((-) :: (Int -> Int -> Int)) a_f_Misc__n (1 :: Int));
                    r_wC_Misc_28_=((:) r_f_Misc__p r_f_Misc__ps,r_f_Misc__rest2)
                 } in  f_id_THEPRIME r_wC_Misc_28_
         } in  r_wC_Misc_27_;
    f_f_Misc_readlist [] a_wC_Misc_29_=([],(0 :: Int));
    f_f_Misc_readlist (a_f_Misc__a:a_f_Misc__l) a_f_Misc__getitem=
        let { 
            (r_f_Misc__p,r_f_Misc__rest)=a_f_Misc__getitem ((:) a_f_Misc__a a_f_Misc__l);
            r_wC_Misc_30_=
                let { 
                    (r_f_Misc__ps,r_f_Misc__n)=f_f_Misc_readlist (f_f_StandardXfer_skipwhitespace r_f_Misc__rest) a_f_Misc__getitem;
                    r_wC_Misc_31_=((:) r_f_Misc__p r_f_Misc__ps,((+) :: (Int -> Int -> Int)) r_f_Misc__n (1 :: Int))
                 } in  f_id_THEPRIME r_wC_Misc_31_
         } in  r_wC_Misc_30_;
    f_f_Misc_writelist [] a_wC_Misc_32_=[];
    f_f_Misc_writelist (a_f_Misc__x:a_f_Misc__xs) a_f_Misc__convert=(++) (a_f_Misc__convert a_f_Misc__x) ((++) "\n" (f_f_Misc_writelist a_f_Misc__xs a_f_Misc__convert));
data 
    T_t_Geom2D_POINT2=F_T_Geom2D_Point2 Double Double;
data 
    T_t_Geom2D_VECTOR2=F_T_Geom2D_Vector2 Double Double;
data 
    T_t_Geom2D_TRANSFORM2=F_T_Geom2D_Transform2 T_t_Geom2D_ROTATE2 T_t_Geom2D_MOVE2;
type 
    T_t_Geom2D_ROTATE2=T_t_Geom2D_VECTOR2;
type 
    T_t_Geom2D_MOVE2=T_t_Geom2D_VECTOR2;
    f_f_Geom2D_vecmake a_f_Geom2D__x a_f_Geom2D__y=F_T_Geom2D_Vector2 a_f_Geom2D__x a_f_Geom2D__y;
    f_f_Geom2D_vecx (F_T_Geom2D_Vector2 a_f_Geom2D__x a_f_Geom2D__y)=a_f_Geom2D__x;
    f_f_Geom2D_vecy (F_T_Geom2D_Vector2 a_f_Geom2D__x a_f_Geom2D__y)=a_f_Geom2D__y;
    f_f_Geom2D_vecsum (F_T_Geom2D_Vector2 a_f_Geom2D__x1 a_f_Geom2D__y1) (F_T_Geom2D_Vector2 a_f_Geom2D__x2 a_f_Geom2D__y2)=F_T_Geom2D_Vector2 (((+) :: (Double -> Double -> Double)) a_f_Geom2D__x1 a_f_Geom2D__x2) (((+) :: (Double -> Double -> Double)) a_f_Geom2D__y1 a_f_Geom2D__y2);
    f_f_Geom2D_vecdiff (F_T_Geom2D_Vector2 a_f_Geom2D__x1 a_f_Geom2D__y1) (F_T_Geom2D_Vector2 a_f_Geom2D__x2 a_f_Geom2D__y2)=F_T_Geom2D_Vector2 (((-) :: (Double -> Double -> Double)) a_f_Geom2D__x1 a_f_Geom2D__x2) (((-) :: (Double -> Double -> Double)) a_f_Geom2D__y1 a_f_Geom2D__y2);
    f_f_Geom2D_scaladd (F_T_Geom2D_Vector2 a_f_Geom2D__x a_f_Geom2D__y) a_f_Geom2D__s=F_T_Geom2D_Vector2 (((+) :: (Double -> Double -> Double)) a_f_Geom2D__x a_f_Geom2D__s) (((+) :: (Double -> Double -> Double)) a_f_Geom2D__y a_f_Geom2D__s);
    f_f_Geom2D_scalsub (F_T_Geom2D_Vector2 a_f_Geom2D__x a_f_Geom2D__y) a_f_Geom2D__s=F_T_Geom2D_Vector2 (((-) :: (Double -> Double -> Double)) a_f_Geom2D__x a_f_Geom2D__s) (((-) :: (Double -> Double -> Double)) a_f_Geom2D__y a_f_Geom2D__s);
    f_f_Geom2D_scalmult (F_T_Geom2D_Vector2 a_f_Geom2D__x a_f_Geom2D__y) a_f_Geom2D__s=F_T_Geom2D_Vector2 (((*) :: (Double -> Double -> Double)) a_f_Geom2D__x a_f_Geom2D__s) (((*) :: (Double -> Double -> Double)) a_f_Geom2D__y a_f_Geom2D__s);
    f_f_Geom2D_scaldiv (F_T_Geom2D_Vector2 a_f_Geom2D__x a_f_Geom2D__y) a_f_Geom2D__s=F_T_Geom2D_Vector2 (((/) :: (Double -> Double -> Double)) a_f_Geom2D__x a_f_Geom2D__s) (((/) :: (Double -> Double -> Double)) a_f_Geom2D__y a_f_Geom2D__s);
    f_f_Geom2D_vecinner (F_T_Geom2D_Vector2 a_f_Geom2D__x1 a_f_Geom2D__y1) (F_T_Geom2D_Vector2 a_f_Geom2D__x2 a_f_Geom2D__y2)=((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom2D__x1 a_f_Geom2D__x2) (((*) :: (Double -> Double -> Double)) a_f_Geom2D__y1 a_f_Geom2D__y2);
    f_f_Geom2D_veclen (F_T_Geom2D_Vector2 a_f_Geom2D__x1 a_f_Geom2D__y1)=f_f_StandardReals_sqrt (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom2D__x1 a_f_Geom2D__x1) (((*) :: (Double -> Double -> Double)) a_f_Geom2D__y1 a_f_Geom2D__y1));
    f_f_Geom2D_vecprod (F_T_Geom2D_Vector2 a_f_Geom2D__x1 a_f_Geom2D__y1) (F_T_Geom2D_Vector2 a_f_Geom2D__x2 a_f_Geom2D__y2)=F_T_Geom2D_Vector2 (((-) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom2D__x1 a_f_Geom2D__y2) (((*) :: (Double -> Double -> Double)) a_f_Geom2D__y1 a_f_Geom2D__x2)) (((-) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom2D__x1 a_f_Geom2D__y2) 
        (((*) :: (Double -> Double -> Double)) a_f_Geom2D__y1 a_f_Geom2D__x2));
    f_f_Geom2D_vecnorm (F_T_Geom2D_Vector2 a_f_Geom2D__x a_f_Geom2D__y)=
        let { 
            r_f_Geom2D__l=f_f_Geom2D_veclen (F_T_Geom2D_Vector2 a_f_Geom2D__x a_f_Geom2D__y);
            r_wC_Geom2D_0_=F_T_Geom2D_Vector2 (((/) :: (Double -> Double -> Double)) a_f_Geom2D__x r_f_Geom2D__l) (((/) :: (Double -> Double -> Double)) a_f_Geom2D__y r_f_Geom2D__l)
         } in  r_wC_Geom2D_0_;
    f_f_Geom2D_ptmake a_f_Geom2D__x a_f_Geom2D__y=F_T_Geom2D_Point2 a_f_Geom2D__x a_f_Geom2D__y;
    f_f_Geom2D_ptx (F_T_Geom2D_Point2 a_f_Geom2D__x a_f_Geom2D__y)=a_f_Geom2D__x;
    f_f_Geom2D_pty (F_T_Geom2D_Point2 a_f_Geom2D__x a_f_Geom2D__y)=a_f_Geom2D__y;
    f_f_Geom2D_ptsdist (F_T_Geom2D_Point2 a_f_Geom2D__x1 a_f_Geom2D__y1) (F_T_Geom2D_Point2 a_f_Geom2D__x2 a_f_Geom2D__y2)=
        let { 
            r_f_Geom2D__dx=((-) :: (Double -> Double -> Double)) a_f_Geom2D__x1 a_f_Geom2D__x2;
            r_wC_Geom2D_1_=
                let { 
                    r_f_Geom2D__dy=((-) :: (Double -> Double -> Double)) a_f_Geom2D__y1 a_f_Geom2D__y2;
                    r_wC_Geom2D_2_=f_f_StandardReals_sqrt (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) r_f_Geom2D__dx r_f_Geom2D__dx) (((*) :: (Double -> Double -> Double)) r_f_Geom2D__dy r_f_Geom2D__dy))
                 } in  f_id_THEPRIME r_wC_Geom2D_2_
         } in  r_wC_Geom2D_1_;
    f_f_Geom2D_tranmake a_f_Geom2D__r a_f_Geom2D__m=F_T_Geom2D_Transform2 a_f_Geom2D__r a_f_Geom2D__m;
    f_f_Geom2D_tranrot (F_T_Geom2D_Transform2 a_f_Geom2D__r a_f_Geom2D__m)=a_f_Geom2D__r;
    f_f_Geom2D_tranmove (F_T_Geom2D_Transform2 a_f_Geom2D__r a_f_Geom2D__m)=a_f_Geom2D__m;
    f_f_Geom2D_vecget a_f_Geom2D__l=
        let { 
            (r_f_Geom2D__x,r_f_Geom2D__rest1)=f_f_Misc_getreal a_f_Geom2D__l;
            r_wC_Geom2D_3_=
                let { 
                    (r_f_Geom2D__y,r_f_Geom2D__rest2)=f_f_Misc_getreal r_f_Geom2D__rest1;
                    r_wC_Geom2D_4_=(F_T_Geom2D_Vector2 r_f_Geom2D__x r_f_Geom2D__y,r_f_Geom2D__rest2)
                 } in  f_id_THEPRIME r_wC_Geom2D_4_
         } in  r_wC_Geom2D_3_;
    f_f_Geom2D_ptget a_f_Geom2D__l=
        let { 
            (r_f_Geom2D__x,r_f_Geom2D__rest1)=f_f_Misc_getreal a_f_Geom2D__l;
            r_wC_Geom2D_5_=
                let { 
                    (r_f_Geom2D__y,r_f_Geom2D__rest2)=f_f_Misc_getreal r_f_Geom2D__rest1;
                    r_wC_Geom2D_6_=(F_T_Geom2D_Point2 r_f_Geom2D__x r_f_Geom2D__y,r_f_Geom2D__rest2)
                 } in  f_id_THEPRIME r_wC_Geom2D_6_
         } in  r_wC_Geom2D_5_;
    f_f_Geom2D_tranget a_f_Geom2D__x=
        let { 
            (r_f_Geom2D__rot,r_f_Geom2D__rest)=f_f_Geom2D_vecget a_f_Geom2D__x;
            r_wC_Geom2D_7_=
                let { 
                    (r_f_Geom2D__mv,r_f_Geom2D__rest2)=f_f_Geom2D_vecget r_f_Geom2D__rest;
                    r_wC_Geom2D_8_=(F_T_Geom2D_Transform2 r_f_Geom2D__rot r_f_Geom2D__mv,r_f_Geom2D__rest2)
                 } in  f_id_THEPRIME r_wC_Geom2D_8_
         } in  r_wC_Geom2D_7_;
    f_f_Geom2D_ptput (F_T_Geom2D_Point2 a_f_Geom2D__x a_f_Geom2D__y)=(++) (f_f_StandardXfer_realtostr a_f_Geom2D__x) ((++) " " (f_f_StandardXfer_realtostr a_f_Geom2D__y));
    f_f_Geom2D_vecput (F_T_Geom2D_Vector2 a_f_Geom2D__x a_f_Geom2D__y)=(++) (f_f_StandardXfer_realtostr a_f_Geom2D__x) ((++) " " (f_f_StandardXfer_realtostr a_f_Geom2D__y));
    f_f_Geom2D_tranput (F_T_Geom2D_Transform2 (F_T_Geom2D_Vector2 a_f_Geom2D__rx a_f_Geom2D__ry) (F_T_Geom2D_Vector2 a_f_Geom2D__mx a_f_Geom2D__my))=(++) (f_f_StandardXfer_realtostr a_f_Geom2D__rx) ((++) " " ((++) (f_f_StandardXfer_realtostr a_f_Geom2D__ry) ((++) " " 
        ((++) (f_f_StandardXfer_realtostr a_f_Geom2D__mx) ((++) " " ((++) (f_f_StandardXfer_realtostr a_f_Geom2D__my) " "))))));
    f_f_StandardReals_sqrt a_x=((sqrt) :: (Double -> Double)) a_x;
data 
    T_t_Gd_GDT t1=F_T_Gd_Terminal t1 | F_T_Gd_Interior [(T_t_Gd_GDT t1)];
    f_f_Gd_is_terminal (F_T_Gd_Terminal a_wC_Gd_0_)=True;
    f_f_Gd_is_terminal (F_T_Gd_Interior a_wC_Gd_1_)=False;
    f_f_Gd_find_terminal (F_T_Gd_Terminal a_f_Gd__x)=a_f_Gd__x;
    f_f_Gd_find_interior (F_T_Gd_Interior a_f_Gd__x)=a_f_Gd__x;
    f_f_Gd_map_GDTtraverse a_f_Gd__s a_f_Gd__c []=[];
    f_f_Gd_map_GDTtraverse a_f_Gd__s a_f_Gd__c (a_f_Gd__a:a_f_Gd__r)=(:) (f_f_Gd_GDTtraverse a_f_Gd__a a_f_Gd__s a_f_Gd__c) (f_f_Gd_map_GDTtraverse a_f_Gd__s a_f_Gd__c a_f_Gd__r);
    f_f_Gd_map_GDTtraverse_1 a_f_Gd__s a_f_Gd__c [] a_f_Gd__q=[];
    f_f_Gd_map_GDTtraverse_1 a_f_Gd__s a_f_Gd__c (a_f_Gd__a:a_f_Gd__r) a_f_Gd__q=(:) (f_f_Gd_GDTtraverse_1 a_f_Gd__a a_f_Gd__q a_f_Gd__s a_f_Gd__c) (f_f_Gd_map_GDTtraverse_1 a_f_Gd__s a_f_Gd__c a_f_Gd__r a_f_Gd__q);
    f_f_Gd_GDTtraverse (F_T_Gd_Terminal a_f_Gd__x) a_f_Gd__solve a_f_Gd__combine=a_f_Gd__solve a_f_Gd__x;
    f_f_Gd_GDTtraverse (F_T_Gd_Interior a_f_Gd__x) a_f_Gd__solve a_f_Gd__combine=a_f_Gd__combine (f_f_Gd_map_GDTtraverse a_f_Gd__solve a_f_Gd__combine a_f_Gd__x);
    f_f_Gd_GDTtraverse_1 (F_T_Gd_Terminal a_f_Gd__x) a_f_Gd__query a_f_Gd__solve a_f_Gd__combine=a_f_Gd__solve a_f_Gd__x a_f_Gd__query;
    f_f_Gd_GDTtraverse_1 (F_T_Gd_Interior a_f_Gd__x) a_f_Gd__query a_f_Gd__solve a_f_Gd__combine=a_f_Gd__combine (f_f_Gd_map_GDTtraverse_1 a_f_Gd__solve a_f_Gd__combine a_f_Gd__x a_f_Gd__query);
    f_f_Gd_GDTcreate a_f_Gd__x a_f_Gd__leaf a_f_Gd__divide a_f_Gd__solve=
        let { 
            r_wC_Gd_2_=F_T_Gd_Terminal (a_f_Gd__solve a_f_Gd__x);
            r_wC_Gd_3_=F_T_Gd_Interior (f_f_Gd_map_GDTcreate a_f_Gd__leaf a_f_Gd__divide a_f_Gd__solve (a_f_Gd__divide a_f_Gd__x))
         } in  
            if (a_f_Gd__leaf a_f_Gd__x)
            then r_wC_Gd_2_
            else 
                r_wC_Gd_3_;
    f_f_Gd_GDTcreate_1 a_f_Gd__x a_f_Gd__leaf a_f_Gd__divide a_f_Gd__solve a_f_Gd__maxdepth=f_f_Gd_Gdt2Create a_f_Gd__x a_f_Gd__leaf a_f_Gd__divide a_f_Gd__solve a_f_Gd__maxdepth (0 :: Int);
    f_f_Gd_Gdt2Create a_f_Gd__x a_f_Gd__leaf a_f_Gd__divide a_f_Gd__solve a_f_Gd__maxdepth a_f_Gd__depth=
        let { 
            r_wC_Gd_4_=F_T_Gd_Terminal (a_f_Gd__solve a_f_Gd__x);
            r_wC_Gd_5_=F_T_Gd_Interior (f_f_Gd_map_Gdt2Create a_f_Gd__leaf a_f_Gd__divide a_f_Gd__solve (a_f_Gd__divide a_f_Gd__x) a_f_Gd__maxdepth (((+) :: (Int -> Int -> Int)) a_f_Gd__depth (1 :: Int)))
         } in  
            if (
                if (a_f_Gd__leaf a_f_Gd__x)
                then True
                else 
                    (((==) :: (Int -> Int -> Bool)) a_f_Gd__depth a_f_Gd__maxdepth))
            then r_wC_Gd_4_
            else 
                r_wC_Gd_5_;
    f_f_Gd_map_GDTcreate a_f_Gd__l a_f_Gd__d a_f_Gd__s []=[];
    f_f_Gd_map_GDTcreate a_f_Gd__l a_f_Gd__d a_f_Gd__s (a_f_Gd__a:a_f_Gd__r)=(:) (f_f_Gd_GDTcreate a_f_Gd__a a_f_Gd__l a_f_Gd__d a_f_Gd__s) (f_f_Gd_map_GDTcreate a_f_Gd__l a_f_Gd__d a_f_Gd__s a_f_Gd__r);
    f_f_Gd_map_Gdt2Create a_f_Gd__l a_f_Gd__d a_f_Gd__s [] a_f_Gd__m a_f_Gd__dpt=[];
    f_f_Gd_map_Gdt2Create a_f_Gd__l a_f_Gd__d a_f_Gd__s (a_f_Gd__a:a_f_Gd__r) a_f_Gd__m a_f_Gd__dpt=(:) (f_f_Gd_Gdt2Create a_f_Gd__a a_f_Gd__l a_f_Gd__d a_f_Gd__s a_f_Gd__m a_f_Gd__dpt) (f_f_Gd_map_Gdt2Create a_f_Gd__l a_f_Gd__d a_f_Gd__s a_f_Gd__r a_f_Gd__m a_f_Gd__dpt);
    f_f_Gd_find_subtrees 0 a_f_Gd__stack=([],a_f_Gd__stack);
    f_f_Gd_find_subtrees a_f_Gd__n a_f_Gd__stack=
        let { 
            (r_f_Gd__others,r_f_Gd__rest)=f_f_Gd_find_subtrees (((-) :: (Int -> Int -> Int)) a_f_Gd__n (1 :: Int)) (f_f_Misc_tl a_f_Gd__stack);
            r_wC_Gd_6_=((++) r_f_Gd__others ((:) (f_f_Misc_hd a_f_Gd__stack) []),r_f_Gd__rest)
         } in  r_wC_Gd_6_;
    f_f_Gd_make_gdt a_wC_Gd_7_ a_f_Gd__stack a_f_Gd__instream [] a_wC_Gd_8_=(f_f_Misc_hd a_f_Gd__stack,a_f_Gd__instream);
    f_f_Gd_make_gdt a_f_Gd__f a_f_Gd__stack a_f_Gd__instream (a_f_Gd__node:a_f_Gd__restofnodes) a_f_Gd__get_leaf=
        let { 
            (r_f_Gd__geom,r_f_Gd__rest)=a_f_Gd__get_leaf a_f_Gd__instream;
            r_wC_Gd_9_=
                let { 
                    (r_f_Gd__subtrees,r_f_Gd__restofstack)=f_f_Gd_find_subtrees a_f_Gd__f a_f_Gd__stack;
                    r_wC_Gd_10_=
                        let { 
                            r_wC_Gd_11_=f_f_Gd_make_gdt a_f_Gd__f ((:) (F_T_Gd_Terminal r_f_Gd__geom) a_f_Gd__stack) r_f_Gd__rest a_f_Gd__restofnodes a_f_Gd__get_leaf;
                            r_wC_Gd_12_=f_f_Gd_make_gdt a_f_Gd__f ((:) (F_T_Gd_Interior r_f_Gd__subtrees) r_f_Gd__restofstack) a_f_Gd__instream a_f_Gd__restofnodes a_f_Gd__get_leaf
                         } in  
                            if (f_eq_c a_f_Gd__node 't')
                            then r_wC_Gd_11_
                            else 
                                r_wC_Gd_12_
                 } in  f_id_THEPRIME r_wC_Gd_10_
         } in  r_wC_Gd_9_;
    f_f_Gd_GDTget a_f_Gd__instream a_f_Gd__read_leaf=
        let { 
            (r_f_Gd__f,r_f_Gd__rest0)=f_f_Misc_getinteger a_f_Gd__instream;
            r_wC_Gd_13_=
                let { 
                    (r_f_Gd__n_cnt,r_f_Gd__rest1)=f_f_Misc_getinteger r_f_Gd__rest0;
                    r_wC_Gd_14_=
                        let { 
                            (r_f_Gd__n_list,r_f_Gd__rest2)=f_f_Misc_getstring r_f_Gd__rest1 r_f_Gd__n_cnt;
                            r_wC_Gd_15_=
                                let { 
                                    (r_f_Gd__t_cnt,r_f_Gd__rest3)=f_f_Misc_getinteger r_f_Gd__rest2;
                                    r_wC_Gd_16_=f_f_Gd_make_gdt r_f_Gd__f [] r_f_Gd__rest3 r_f_Gd__n_list a_f_Gd__read_leaf
                                 } in  f_id_THEPRIME r_wC_Gd_16_
                         } in  f_id_THEPRIME r_wC_Gd_15_
                 } in  f_id_THEPRIME r_wC_Gd_14_
         } in  r_wC_Gd_13_;
    f_f_Gd_prepare_save (F_T_Gd_Terminal a_f_Gd__x) a_f_Gd__convert=((1 :: Int),(1 :: Int),a_f_Gd__convert a_f_Gd__x,(:) 't' []);
    f_f_Gd_prepare_save (F_T_Gd_Interior a_f_Gd__x) a_f_Gd__convert=
        let { 
            (r_f_Gd__t_cnt,r_f_Gd__n_cnt,r_f_Gd__t_lst,r_f_Gd__n_lst)=f_f_Gd_prepare_subtrees a_f_Gd__x a_f_Gd__convert;
            r_wC_Gd_17_=(r_f_Gd__t_cnt,((+) :: (Int -> Int -> Int)) r_f_Gd__n_cnt (1 :: Int),r_f_Gd__t_lst,(++) r_f_Gd__n_lst ((:) 'd' []))
         } in  r_wC_Gd_17_;
    f_f_Gd_prepare_subtrees [] a_wC_Gd_18_=((0 :: Int),(0 :: Int),[],[]);
    f_f_Gd_prepare_subtrees (a_f_Gd__t:a_f_Gd__xt) a_f_Gd__convert=
        let { 
            (r_f_Gd__t_cnt1,r_f_Gd__n_cnt1,r_f_Gd__t_lst1,r_f_Gd__n_lst1)=f_f_Gd_prepare_save a_f_Gd__t a_f_Gd__convert;
            r_wC_Gd_19_=
                let { 
                    (r_f_Gd__t_cnt2,r_f_Gd__n_cnt2,r_f_Gd__t_lst2,r_f_Gd__n_lst2)=f_f_Gd_prepare_subtrees a_f_Gd__xt a_f_Gd__convert;
                    r_wC_Gd_20_=(((+) :: (Int -> Int -> Int)) r_f_Gd__t_cnt1 r_f_Gd__t_cnt2,((+) :: (Int -> Int -> Int)) r_f_Gd__n_cnt1 r_f_Gd__n_cnt2,(++) r_f_Gd__t_lst1 r_f_Gd__t_lst2,(++) r_f_Gd__n_lst1 r_f_Gd__n_lst2)
                 } in  f_id_THEPRIME r_wC_Gd_20_
         } in  r_wC_Gd_19_;
    f_f_Gd_GDTput a_f_Gd__tree a_f_Gd__f a_f_Gd__outstream a_f_Gd__convert_geom=
        let { 
            (r_f_Gd__term_cnt,r_f_Gd__node_cnt,r_f_Gd__term_list,r_f_Gd__node_list)=f_f_Gd_prepare_save a_f_Gd__tree a_f_Gd__convert_geom;
            r_wC_Gd_21_=(++) a_f_Gd__outstream ((++) (f_f_StandardXfer_numtostr a_f_Gd__f) ((++) "\n" ((++) (f_f_StandardXfer_numtostr r_f_Gd__node_cnt) 
                ((++) "\n" ((++) r_f_Gd__node_list ((++) "\n" ((++) (f_f_StandardXfer_numtostr r_f_Gd__term_cnt) ((++) "\n" r_f_Gd__term_list))))))))
         } in  r_wC_Gd_21_;
data 
    T_t_Csg_CSG t1 t2=C_T_Csg_Emptysolid | C_T_Csg_Fullsolid | F_T_Csg_Primitive t1 | F_T_Csg_Compose (T_t_Csg_CSG t1 t2) t2 (T_t_Csg_CSG t1 t2);
    f_f_Csg_prims_in (F_T_Csg_Primitive a_wC_Csg_0_)=(1 :: Int);
    f_f_Csg_prims_in (F_T_Csg_Compose a_f_Csg__l a_wC_Csg_1_ a_f_Csg__r)=((+) :: (Int -> Int -> Int)) (f_f_Csg_prims_in a_f_Csg__l) (f_f_Csg_prims_in a_f_Csg__r);
    f_f_Csg_prims_in C_T_Csg_Emptysolid=(0 :: Int);
    f_f_Csg_prims_in C_T_Csg_Fullsolid=(0 :: Int);
    f_f_Csg_is_prim C_T_Csg_Emptysolid=False;
    f_f_Csg_is_prim C_T_Csg_Fullsolid=False;
    f_f_Csg_is_prim (F_T_Csg_Primitive a_wC_Csg_2_)=True;
    f_f_Csg_is_prim (F_T_Csg_Compose a_wC_Csg_3_ a_wC_Csg_4_ a_wC_Csg_5_)=False;
    f_f_Csg_is_empty C_T_Csg_Emptysolid=True;
    f_f_Csg_is_empty C_T_Csg_Fullsolid=False;
    f_f_Csg_is_empty (F_T_Csg_Primitive a_wC_Csg_6_)=False;
    f_f_Csg_is_empty (F_T_Csg_Compose a_wC_Csg_7_ a_wC_Csg_8_ a_wC_Csg_9_)=False;
    f_f_Csg_is_full C_T_Csg_Emptysolid=False;
    f_f_Csg_is_full C_T_Csg_Fullsolid=True;
    f_f_Csg_is_full (F_T_Csg_Primitive a_wC_Csg_10_)=False;
    f_f_Csg_is_full (F_T_Csg_Compose a_wC_Csg_11_ a_wC_Csg_12_ a_wC_Csg_13_)=False;
    f_f_Csg_is_composed C_T_Csg_Emptysolid=False;
    f_f_Csg_is_composed C_T_Csg_Fullsolid=False;
    f_f_Csg_is_composed (F_T_Csg_Primitive a_wC_Csg_14_)=False;
    f_f_Csg_is_composed (F_T_Csg_Compose a_wC_Csg_15_ a_wC_Csg_16_ a_wC_Csg_17_)=True;
    f_f_Csg_CSGtraverse a_f_Csg__query (F_T_Csg_Primitive a_f_Csg__prim) a_f_Csg__solve a_wC_Csg_18_=a_f_Csg__solve a_f_Csg__prim a_f_Csg__query;
    f_f_Csg_CSGtraverse a_f_Csg__query (F_T_Csg_Compose a_f_Csg__l a_f_Csg__op a_f_Csg__r) a_f_Csg__solve a_f_Csg__combine=a_f_Csg__combine (f_f_Csg_CSGtraverse a_f_Csg__query a_f_Csg__l a_f_Csg__solve a_f_Csg__combine) a_f_Csg__op (f_f_Csg_CSGtraverse a_f_Csg__query a_f_Csg__r a_f_Csg__solve a_f_Csg__combine);
    f_f_Csg_CSGtraverse_1 a_wC_Csg_19_ C_T_Csg_Fullsolid a_wC_Csg_20_ a_wC_Csg_21_ a_f_Csg__fullsolve=a_f_Csg__fullsolve;
    f_f_Csg_CSGtraverse_1 a_f_Csg__query (F_T_Csg_Primitive a_f_Csg__prim) a_f_Csg__solve a_wC_Csg_22_ a_wC_Csg_23_=a_f_Csg__solve a_f_Csg__prim a_f_Csg__query;
    f_f_Csg_CSGtraverse_1 a_f_Csg__query (F_T_Csg_Compose a_f_Csg__l a_f_Csg__op a_f_Csg__r) a_f_Csg__solve a_f_Csg__combine a_f_Csg__full=a_f_Csg__combine (f_f_Csg_CSGtraverse_1 a_f_Csg__query a_f_Csg__l a_f_Csg__solve a_f_Csg__combine a_f_Csg__full) a_f_Csg__op (f_f_Csg_CSGtraverse_1 a_f_Csg__query a_f_Csg__r a_f_Csg__solve a_f_Csg__combine a_f_Csg__full);
    f_f_Csg_CSGtraverse_2 a_wC_Csg_24_ C_T_Csg_Fullsolid a_wC_Csg_25_ a_wC_Csg_26_ a_f_Csg__fullsolve a_wC_Csg_27_=a_f_Csg__fullsolve;
    f_f_Csg_CSGtraverse_2 a_wC_Csg_28_ C_T_Csg_Emptysolid a_wC_Csg_29_ a_wC_Csg_30_ a_wC_Csg_31_ a_f_Csg__emptysolve=a_f_Csg__emptysolve;
    f_f_Csg_CSGtraverse_2 a_f_Csg__query (F_T_Csg_Primitive a_f_Csg__prim) a_f_Csg__solve a_wC_Csg_32_ a_wC_Csg_33_ a_wC_Csg_34_=a_f_Csg__solve a_f_Csg__prim a_f_Csg__query;
    f_f_Csg_CSGtraverse_2 a_f_Csg__query (F_T_Csg_Compose a_f_Csg__l a_f_Csg__op a_f_Csg__r) a_f_Csg__solve a_f_Csg__combine a_f_Csg__full a_f_Csg__empty=a_f_Csg__combine (f_f_Csg_CSGtraverse_2 a_f_Csg__query a_f_Csg__l a_f_Csg__solve a_f_Csg__combine a_f_Csg__full a_f_Csg__empty) a_f_Csg__op (f_f_Csg_CSGtraverse_2 a_f_Csg__query a_f_Csg__r a_f_Csg__solve a_f_Csg__combine a_f_Csg__full a_f_Csg__empty);
    f_f_Csg_CSGtraverse_3 (F_T_Csg_Primitive a_f_Csg__prim) a_f_Csg__solve a_wC_Csg_35_=a_f_Csg__solve a_f_Csg__prim;
    f_f_Csg_CSGtraverse_3 (F_T_Csg_Compose a_f_Csg__l a_f_Csg__op a_f_Csg__r) a_f_Csg__solve a_f_Csg__combine=a_f_Csg__combine (f_f_Csg_CSGtraverse_3 a_f_Csg__l a_f_Csg__solve a_f_Csg__combine) a_f_Csg__op (f_f_Csg_CSGtraverse_3 a_f_Csg__r a_f_Csg__solve a_f_Csg__combine);
    f_f_Csg_CSGtraverse_4 C_T_Csg_Fullsolid a_wC_Csg_36_ a_wC_Csg_37_ a_f_Csg__fullsolve=a_f_Csg__fullsolve;
    f_f_Csg_CSGtraverse_4 (F_T_Csg_Primitive a_f_Csg__prim) a_f_Csg__solve a_wC_Csg_38_ a_wC_Csg_39_=a_f_Csg__solve a_f_Csg__prim;
    f_f_Csg_CSGtraverse_4 (F_T_Csg_Compose a_f_Csg__l a_f_Csg__op a_f_Csg__r) a_f_Csg__solve a_f_Csg__combine a_f_Csg__full=a_f_Csg__combine (f_f_Csg_CSGtraverse_4 a_f_Csg__l a_f_Csg__solve a_f_Csg__combine a_f_Csg__full) a_f_Csg__op (f_f_Csg_CSGtraverse_4 a_f_Csg__r a_f_Csg__solve a_f_Csg__combine a_f_Csg__full);
    f_f_Csg_CSGtraverse_5 C_T_Csg_Emptysolid a_wC_Csg_40_ a_wC_Csg_41_ a_wC_Csg_42_ a_f_Csg__emptysolve=a_f_Csg__emptysolve;
    f_f_Csg_CSGtraverse_5 C_T_Csg_Fullsolid a_wC_Csg_43_ a_wC_Csg_44_ a_f_Csg__fullsolve a_wC_Csg_45_=a_f_Csg__fullsolve;
    f_f_Csg_CSGtraverse_5 (F_T_Csg_Primitive a_f_Csg__prim) a_f_Csg__solve a_wC_Csg_46_ a_wC_Csg_47_ a_wC_Csg_48_=a_f_Csg__solve a_f_Csg__prim;
    f_f_Csg_CSGtraverse_5 (F_T_Csg_Compose a_f_Csg__l a_f_Csg__op a_f_Csg__r) a_f_Csg__solve a_f_Csg__combine a_f_Csg__full a_f_Csg__empty=a_f_Csg__combine (f_f_Csg_CSGtraverse_5 a_f_Csg__l a_f_Csg__solve a_f_Csg__combine a_f_Csg__full a_f_Csg__empty) a_f_Csg__op (f_f_Csg_CSGtraverse_5 a_f_Csg__r a_f_Csg__solve a_f_Csg__combine a_f_Csg__full a_f_Csg__empty);
    f_f_Csg_make_csg a_f_Csg__stack a_f_Csg__instream [] a_wC_Csg_49_ a_wC_Csg_50_=(f_f_Misc_hd a_f_Csg__stack,a_f_Csg__instream);
    f_f_Csg_make_csg a_f_Csg__stack a_f_Csg__instream (a_f_Csg__node:a_f_Csg__restofnodes) a_f_Csg__get_prim a_f_Csg__get_rop=
        let { 
            (r_f_Csg__prim,r_f_Csg__rest)=a_f_Csg__get_prim a_f_Csg__instream;
            r_wC_Csg_51_=
                let { 
                    r_f_Csg__r=f_f_Misc_hd a_f_Csg__stack;
                    r_wC_Csg_52_=
                        let { 
                            r_f_Csg__t=f_f_Misc_tl a_f_Csg__stack;
                            r_wC_Csg_53_=
                                let { 
                                    r_wC_Csg_54_=f_f_Csg_make_csg ((:) (F_T_Csg_Primitive r_f_Csg__prim) a_f_Csg__stack) r_f_Csg__rest a_f_Csg__restofnodes a_f_Csg__get_prim a_f_Csg__get_rop;
                                    r_wC_Csg_55_=
                                        let { 
                                            r_wC_Csg_56_=f_f_Csg_make_csg ((:) C_T_Csg_Emptysolid a_f_Csg__stack) a_f_Csg__instream a_f_Csg__restofnodes a_f_Csg__get_prim a_f_Csg__get_rop;
                                            r_wC_Csg_57_=
                                                let { 
                                                    r_wC_Csg_58_=f_f_Csg_make_csg ((:) C_T_Csg_Fullsolid a_f_Csg__stack) a_f_Csg__instream a_f_Csg__restofnodes a_f_Csg__get_prim a_f_Csg__get_rop;
                                                    r_wC_Csg_59_=f_f_Csg_make_csg ((:) (F_T_Csg_Compose (f_f_Misc_hd r_f_Csg__t) (a_f_Csg__get_rop a_f_Csg__node) r_f_Csg__r) (f_f_Misc_tl r_f_Csg__t)) a_f_Csg__instream a_f_Csg__restofnodes a_f_Csg__get_prim a_f_Csg__get_rop
                                                 } in  
                                                    if (f_eq_c a_f_Csg__node 'f')
                                                    then r_wC_Csg_58_
                                                    else 
                                                        r_wC_Csg_59_
                                         } in  
                                            if (f_eq_c a_f_Csg__node 'e')
                                            then r_wC_Csg_56_
                                            else 
                                                r_wC_Csg_57_
                                 } in  
                                    if (f_eq_c a_f_Csg__node 'h')
                                    then r_wC_Csg_54_
                                    else 
                                        r_wC_Csg_55_
                         } in  f_id_THEPRIME r_wC_Csg_53_
                 } in  f_id_THEPRIME r_wC_Csg_52_
         } in  r_wC_Csg_51_;
    f_f_Csg_CSGget a_f_Csg__instream a_f_Csg__read_prim a_f_Csg__read_rop=
        let { 
            (r_f_Csg__t_cnt,r_f_Csg__rest1)=f_f_Misc_getinteger a_f_Csg__instream;
            r_wC_Csg_60_=
                let { 
                    (r_f_Csg__t_list,r_f_Csg__rest2)=f_f_Misc_getstring r_f_Csg__rest1 r_f_Csg__t_cnt;
                    r_wC_Csg_61_=
                        let { 
                            (r_wC_Csg_63_,r_f_Csg__rest3)=f_f_Misc_getinteger r_f_Csg__rest2;
                            r_wC_Csg_62_=f_f_Csg_make_csg [] r_f_Csg__rest3 r_f_Csg__t_list a_f_Csg__read_prim a_f_Csg__read_rop
                         } in  f_id_THEPRIME r_wC_Csg_62_
                 } in  f_id_THEPRIME r_wC_Csg_61_
         } in  r_wC_Csg_60_;
    f_f_Csg_prepare_save C_T_Csg_Emptysolid a_wC_Csg_64_ a_wC_Csg_65_=((0 :: Int),(1 :: Int),[],(:) 'e' []);
    f_f_Csg_prepare_save C_T_Csg_Fullsolid a_wC_Csg_66_ a_wC_Csg_67_=((0 :: Int),(1 :: Int),[],(:) 'f' []);
    f_f_Csg_prepare_save (F_T_Csg_Primitive a_f_Csg__x) a_f_Csg__putprim a_wC_Csg_68_=((1 :: Int),(1 :: Int),a_f_Csg__putprim a_f_Csg__x,(:) 'h' []);
    f_f_Csg_prepare_save (F_T_Csg_Compose a_f_Csg__l a_f_Csg__op a_f_Csg__r) a_f_Csg__putprim a_f_Csg__putrop=
        let { 
            (r_f_Csg__lpcnt,r_f_Csg__lncnt,r_f_Csg__lprimlist,r_f_Csg__lnodelist)=f_f_Csg_prepare_save a_f_Csg__l a_f_Csg__putprim a_f_Csg__putrop;
            r_wC_Csg_69_=
                let { 
                    (r_f_Csg__rpcnt,r_f_Csg__rncnt,r_f_Csg__rprimlist,r_f_Csg__rnodelist)=f_f_Csg_prepare_save a_f_Csg__r a_f_Csg__putprim a_f_Csg__putrop;
                    r_wC_Csg_70_=(((+) :: (Int -> Int -> Int)) r_f_Csg__lpcnt r_f_Csg__rpcnt,((+) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) r_f_Csg__lncnt (1 :: Int)) r_f_Csg__rncnt,(++) r_f_Csg__lprimlist r_f_Csg__rprimlist,(++) r_f_Csg__lnodelist 
                        ((++) r_f_Csg__rnodelist ((:) (a_f_Csg__putrop a_f_Csg__op) [])))
                 } in  f_id_THEPRIME r_wC_Csg_70_
         } in  r_wC_Csg_69_;
    f_f_Csg_CSGput a_f_Csg__tree a_f_Csg__outstream a_f_Csg__convert_prim a_f_Csg__convert_rop=
        let { 
            (r_f_Csg__prim_cnt,r_f_Csg__node_cnt,r_f_Csg__prim_list,r_f_Csg__node_list)=f_f_Csg_prepare_save a_f_Csg__tree a_f_Csg__convert_prim a_f_Csg__convert_rop;
            r_wC_Csg_71_=(++) a_f_Csg__outstream ((++) (f_f_StandardXfer_numtostr r_f_Csg__node_cnt) ((++) "\n" ((++) r_f_Csg__node_list ((++) "\n" 
                ((++) (f_f_StandardXfer_numtostr r_f_Csg__prim_cnt) ((++) "\n" r_f_Csg__prim_list))))))
         } in  r_wC_Csg_71_;
data 
    T_t_Geom3D_POINT3=F_T_Geom3D_Point3 Double Double Double;
data 
    T_t_Geom3D_VECTOR3=F_T_Geom3D_Vector3 Double Double Double;
data 
    T_t_Geom3D_TRANSFORM3=F_T_Geom3D_Transform3 T_t_Geom3D_ROTATE3 T_t_Geom3D_MOVE3;
type 
    T_t_Geom3D_ROTATE3=T_t_Geom3D_VECTOR3;
type 
    T_t_Geom3D_MOVE3=T_t_Geom3D_VECTOR3;
    f_f_Geom3D_vecmake a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z=F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z;
    f_f_Geom3D_vecx (F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z)=a_f_Geom3D__x;
    f_f_Geom3D_vecy (F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z)=a_f_Geom3D__y;
    f_f_Geom3D_vecz (F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z)=a_f_Geom3D__z;
    f_f_Geom3D_vecsum (F_T_Geom3D_Vector3 a_f_Geom3D__x1 a_f_Geom3D__y1 a_f_Geom3D__z1) (F_T_Geom3D_Vector3 a_f_Geom3D__x2 a_f_Geom3D__y2 a_f_Geom3D__z2)=F_T_Geom3D_Vector3 (((+) :: (Double -> Double -> Double)) a_f_Geom3D__x1 a_f_Geom3D__x2) (((+) :: (Double -> Double -> Double)) a_f_Geom3D__y1 a_f_Geom3D__y2) (((+) :: (Double -> Double -> Double)) a_f_Geom3D__z1 a_f_Geom3D__z2);
    f_f_Geom3D_vecdiff (F_T_Geom3D_Vector3 a_f_Geom3D__x1 a_f_Geom3D__y1 a_f_Geom3D__z1) (F_T_Geom3D_Vector3 a_f_Geom3D__x2 a_f_Geom3D__y2 a_f_Geom3D__z2)=F_T_Geom3D_Vector3 (((-) :: (Double -> Double -> Double)) a_f_Geom3D__x1 a_f_Geom3D__x2) (((-) :: (Double -> Double -> Double)) a_f_Geom3D__y1 a_f_Geom3D__y2) (((-) :: (Double -> Double -> Double)) a_f_Geom3D__z1 a_f_Geom3D__z2);
    f_f_Geom3D_scaladd (F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z) a_f_Geom3D__s=F_T_Geom3D_Vector3 (((+) :: (Double -> Double -> Double)) a_f_Geom3D__x a_f_Geom3D__s) (((+) :: (Double -> Double -> Double)) a_f_Geom3D__y a_f_Geom3D__s) (((+) :: (Double -> Double -> Double)) a_f_Geom3D__z a_f_Geom3D__s);
    f_f_Geom3D_scalsub (F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z) a_f_Geom3D__s=F_T_Geom3D_Vector3 (((-) :: (Double -> Double -> Double)) a_f_Geom3D__x a_f_Geom3D__s) (((-) :: (Double -> Double -> Double)) a_f_Geom3D__y a_f_Geom3D__s) (((-) :: (Double -> Double -> Double)) a_f_Geom3D__z a_f_Geom3D__s);
    f_f_Geom3D_scalmult (F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z) a_f_Geom3D__s=F_T_Geom3D_Vector3 (((*) :: (Double -> Double -> Double)) a_f_Geom3D__x a_f_Geom3D__s) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__y a_f_Geom3D__s) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__z a_f_Geom3D__s);
    f_f_Geom3D_scaldiv (F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z) a_f_Geom3D__s=F_T_Geom3D_Vector3 (((/) :: (Double -> Double -> Double)) a_f_Geom3D__x a_f_Geom3D__s) (((/) :: (Double -> Double -> Double)) a_f_Geom3D__y a_f_Geom3D__s) (((/) :: (Double -> Double -> Double)) a_f_Geom3D__z a_f_Geom3D__s);
    f_f_Geom3D_vecinner (F_T_Geom3D_Vector3 a_f_Geom3D__x1 a_f_Geom3D__y1 a_f_Geom3D__z1) (F_T_Geom3D_Vector3 a_f_Geom3D__x2 a_f_Geom3D__y2 a_f_Geom3D__z2)=((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__x1 a_f_Geom3D__x2) (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__y1 a_f_Geom3D__y2) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__z1 a_f_Geom3D__z2));
    f_f_Geom3D_veclen (F_T_Geom3D_Vector3 a_f_Geom3D__x1 a_f_Geom3D__y1 a_f_Geom3D__z1)=f_f_StandardReals_sqrt (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__x1 a_f_Geom3D__x1) (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__y1 a_f_Geom3D__y1) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__z1 a_f_Geom3D__z1)));
    f_f_Geom3D_vecprod (F_T_Geom3D_Vector3 a_f_Geom3D__x1 a_f_Geom3D__y1 a_f_Geom3D__z1) (F_T_Geom3D_Vector3 a_f_Geom3D__x2 a_f_Geom3D__y2 a_f_Geom3D__z2)=F_T_Geom3D_Vector3 (((-) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__y1 a_f_Geom3D__z2) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__z1 a_f_Geom3D__y2)) (((-) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__z1 a_f_Geom3D__x2) 
        (((*) :: (Double -> Double -> Double)) a_f_Geom3D__x1 a_f_Geom3D__z2)) (((-) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__x1 a_f_Geom3D__y2) (((*) :: (Double -> Double -> Double)) a_f_Geom3D__y1 a_f_Geom3D__x2));
    f_f_Geom3D_vecnorm (F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z)=
        let { 
            r_f_Geom3D__l=f_f_Geom3D_veclen (F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z);
            r_wC_Geom3D_0_=F_T_Geom3D_Vector3 (((/) :: (Double -> Double -> Double)) a_f_Geom3D__x r_f_Geom3D__l) (((/) :: (Double -> Double -> Double)) a_f_Geom3D__y r_f_Geom3D__l) (((/) :: (Double -> Double -> Double)) a_f_Geom3D__z r_f_Geom3D__l)
         } in  r_wC_Geom3D_0_;
    f_f_Geom3D_ptmake a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z=F_T_Geom3D_Point3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z;
    f_f_Geom3D_ptx (F_T_Geom3D_Point3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z)=a_f_Geom3D__x;
    f_f_Geom3D_pty (F_T_Geom3D_Point3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z)=a_f_Geom3D__y;
    f_f_Geom3D_ptz (F_T_Geom3D_Point3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z)=a_f_Geom3D__z;
    f_f_Geom3D_ptsdist (F_T_Geom3D_Point3 a_f_Geom3D__x1 a_f_Geom3D__y1 a_f_Geom3D__z1) (F_T_Geom3D_Point3 a_f_Geom3D__x2 a_f_Geom3D__y2 a_f_Geom3D__z2)=
        let { 
            r_f_Geom3D__dx=((-) :: (Double -> Double -> Double)) a_f_Geom3D__x1 a_f_Geom3D__x2;
            r_wC_Geom3D_1_=
                let { 
                    r_f_Geom3D__dy=((-) :: (Double -> Double -> Double)) a_f_Geom3D__y1 a_f_Geom3D__y2;
                    r_wC_Geom3D_2_=
                        let { 
                            r_f_Geom3D__dz=((-) :: (Double -> Double -> Double)) a_f_Geom3D__z1 a_f_Geom3D__z2;
                            r_wC_Geom3D_3_=f_f_StandardReals_sqrt (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) r_f_Geom3D__dx r_f_Geom3D__dx) (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) r_f_Geom3D__dy r_f_Geom3D__dy) (((*) :: (Double -> Double -> Double)) r_f_Geom3D__dz r_f_Geom3D__dz)))
                         } in  f_id_THEPRIME r_wC_Geom3D_3_
                 } in  f_id_THEPRIME r_wC_Geom3D_2_
         } in  r_wC_Geom3D_1_;
    f_f_Geom3D_tranmake a_f_Geom3D__r a_f_Geom3D__m=F_T_Geom3D_Transform3 a_f_Geom3D__r a_f_Geom3D__m;
    f_f_Geom3D_tranrot (F_T_Geom3D_Transform3 a_f_Geom3D__r a_f_Geom3D__m)=a_f_Geom3D__r;
    f_f_Geom3D_tranmove (F_T_Geom3D_Transform3 a_f_Geom3D__r a_f_Geom3D__m)=a_f_Geom3D__m;
    f_f_Geom3D_vecget a_f_Geom3D__l=
        let { 
            (r_f_Geom3D__x,r_f_Geom3D__rest1)=f_f_Misc_getreal a_f_Geom3D__l;
            r_wC_Geom3D_4_=
                let { 
                    (r_f_Geom3D__y,r_f_Geom3D__rest2)=f_f_Misc_getreal r_f_Geom3D__rest1;
                    r_wC_Geom3D_5_=
                        let { 
                            (r_f_Geom3D__z,r_f_Geom3D__rest3)=f_f_Misc_getreal r_f_Geom3D__rest2;
                            r_wC_Geom3D_6_=(F_T_Geom3D_Vector3 r_f_Geom3D__x r_f_Geom3D__y r_f_Geom3D__z,r_f_Geom3D__rest3)
                         } in  f_id_THEPRIME r_wC_Geom3D_6_
                 } in  f_id_THEPRIME r_wC_Geom3D_5_
         } in  r_wC_Geom3D_4_;
    f_f_Geom3D_ptget a_f_Geom3D__l=
        let { 
            (r_f_Geom3D__x,r_f_Geom3D__rest1)=f_f_Misc_getreal a_f_Geom3D__l;
            r_wC_Geom3D_7_=
                let { 
                    (r_f_Geom3D__y,r_f_Geom3D__rest2)=f_f_Misc_getreal r_f_Geom3D__rest1;
                    r_wC_Geom3D_8_=
                        let { 
                            (r_f_Geom3D__z,r_f_Geom3D__rest3)=f_f_Misc_getreal r_f_Geom3D__rest2;
                            r_wC_Geom3D_9_=(F_T_Geom3D_Point3 r_f_Geom3D__x r_f_Geom3D__y r_f_Geom3D__z,r_f_Geom3D__rest3)
                         } in  f_id_THEPRIME r_wC_Geom3D_9_
                 } in  f_id_THEPRIME r_wC_Geom3D_8_
         } in  r_wC_Geom3D_7_;
    f_f_Geom3D_tranget a_f_Geom3D__x=
        let { 
            (r_f_Geom3D__rot,r_f_Geom3D__rest)=f_f_Geom3D_vecget a_f_Geom3D__x;
            r_wC_Geom3D_10_=
                let { 
                    (r_f_Geom3D__mv,r_f_Geom3D__rest2)=f_f_Geom3D_vecget r_f_Geom3D__rest;
                    r_wC_Geom3D_11_=(F_T_Geom3D_Transform3 r_f_Geom3D__rot r_f_Geom3D__mv,r_f_Geom3D__rest2)
                 } in  f_id_THEPRIME r_wC_Geom3D_11_
         } in  r_wC_Geom3D_10_;
    f_f_Geom3D_ptput (F_T_Geom3D_Point3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z)=(++) (f_f_StandardXfer_realtostr a_f_Geom3D__x) ((++) " " ((++) (f_f_StandardXfer_realtostr a_f_Geom3D__y) ((++) " " 
        (f_f_StandardXfer_realtostr a_f_Geom3D__z))));
    f_f_Geom3D_vecput (F_T_Geom3D_Vector3 a_f_Geom3D__x a_f_Geom3D__y a_f_Geom3D__z)=(++) (f_f_StandardXfer_realtostr a_f_Geom3D__x) ((++) " " ((++) (f_f_StandardXfer_realtostr a_f_Geom3D__y) ((++) " " 
        (f_f_StandardXfer_realtostr a_f_Geom3D__z))));
    f_f_Geom3D_tranput (F_T_Geom3D_Transform3 (F_T_Geom3D_Vector3 a_f_Geom3D__rx a_f_Geom3D__ry a_f_Geom3D__rz) (F_T_Geom3D_Vector3 a_f_Geom3D__mx a_f_Geom3D__my a_f_Geom3D__mz))=(++) (f_f_StandardXfer_realtostr a_f_Geom3D__rx) ((++) " " ((++) (f_f_StandardXfer_realtostr a_f_Geom3D__ry) ((++) " " 
        ((++) (f_f_StandardXfer_realtostr a_f_Geom3D__rz) ((++) " " ((++) (f_f_StandardXfer_realtostr a_f_Geom3D__mx) ((++) " " ((++) 
        (f_f_StandardXfer_realtostr a_f_Geom3D__my) ((++) " " (f_f_StandardXfer_realtostr a_f_Geom3D__mz))))))))));
data 
    T_t_Hs2_HS2_GEOM=F_T_Hs2_circle T_t_Misc_SCALAR T_t_Geom2D_POINT2 | F_T_Hs2_line T_t_Misc_SCALAR T_t_Misc_SCALAR T_t_Misc_SCALAR;
data 
    T_t_Hs2_ROP=C_T_Hs2_Un | C_T_Hs2_Inter | C_T_Hs2_Diff;
type 
    T_t_Hs2_CI_HS=(T_t_Misc_SCALAR,T_t_Geom2D_POINT2);
type 
    T_t_Hs2_LI_HS=(T_t_Misc_SCALAR,T_t_Misc_SCALAR,T_t_Misc_SCALAR);
type 
    T_t_Hs2_SURFACE=Int;
type 
    T_t_Hs2_HS2=(T_t_Hs2_HS2_GEOM,T_t_Hs2_SURFACE);
    f_f_Hs2_geths a_f_Hs2__x=
        let { 
            (r_f_Hs2__kind,r_f_Hs2__rest)=f_f_Misc_getstring a_f_Hs2__x (2 :: Int);
            r_wC_Hs2_0_=
                let { 
                    (r_f_Hs2__rad,r_f_Hs2__Rest2)=f_f_Misc_getreal r_f_Hs2__rest;
                    r_wC_Hs2_1_=
                        let { 
                            (r_f_Hs2__centre,r_f_Hs2__Rest3)=f_f_Geom2D_ptget r_f_Hs2__Rest2;
                            r_wC_Hs2_2_=
                                let { 
                                    (r_f_Hs2__Surface,r_f_Hs2__Rest4)=f_f_Misc_getinteger r_f_Hs2__Rest3;
                                    r_wC_Hs2_3_=
                                        let { 
                                            (r_f_Hs2__a,r_f_Hs2__rest2)=f_f_Misc_getreal r_f_Hs2__rest;
                                            r_wC_Hs2_4_=
                                                let { 
                                                    (r_f_Hs2__b,r_f_Hs2__rest3)=f_f_Misc_getreal r_f_Hs2__rest2;
                                                    r_wC_Hs2_5_=
                                                        let { 
                                                            (r_f_Hs2__c,r_f_Hs2__rest4)=f_f_Misc_getreal r_f_Hs2__rest3;
                                                            r_wC_Hs2_6_=
                                                                let { 
                                                                    (r_f_Hs2__surface,r_f_Hs2__rest5)=f_f_Misc_getinteger r_f_Hs2__rest4;
                                                                    r_wC_Hs2_7_=
                                                                        let { 
                                                                            r_wC_Hs2_8_=((F_T_Hs2_circle r_f_Hs2__rad r_f_Hs2__centre,r_f_Hs2__Surface),r_f_Hs2__Rest4);
                                                                            r_wC_Hs2_9_=((F_T_Hs2_line r_f_Hs2__a r_f_Hs2__b r_f_Hs2__c,r_f_Hs2__surface),r_f_Hs2__rest5)
                                                                         } in  
                                                                            if (((==) :: ([Char] -> [Char] -> Bool)) r_f_Hs2__kind "ci")
                                                                            then r_wC_Hs2_8_
                                                                            else 
                                                                                r_wC_Hs2_9_
                                                                 } in  f_id_THEPRIME r_wC_Hs2_7_
                                                         } in  f_id_THEPRIME r_wC_Hs2_6_
                                                 } in  f_id_THEPRIME r_wC_Hs2_5_
                                         } in  f_id_THEPRIME r_wC_Hs2_4_
                                 } in  f_id_THEPRIME r_wC_Hs2_3_
                         } in  f_id_THEPRIME r_wC_Hs2_2_
                 } in  f_id_THEPRIME r_wC_Hs2_1_
         } in  r_wC_Hs2_0_;
    f_f_Hs2_getrop a_f_Hs2__c=
        let { 
            r_wC_Hs2_10_=C_T_Hs2_Un;
            r_wC_Hs2_11_=
                let { 
                    r_wC_Hs2_12_=C_T_Hs2_Diff;
                    r_wC_Hs2_13_=C_T_Hs2_Inter
                 } in  
                    if (f_eq_c a_f_Hs2__c 'd')
                    then r_wC_Hs2_12_
                    else 
                        r_wC_Hs2_13_
         } in  
            if (f_eq_c a_f_Hs2__c 'u')
            then r_wC_Hs2_10_
            else 
                r_wC_Hs2_11_;
    f_f_Hs2_puths ((F_T_Hs2_circle a_f_Hs2__rad a_f_Hs2__cent),a_f_Hs2__surf)=(++) "ci " ((++) (f_f_StandardXfer_realtostr a_f_Hs2__rad) ((++) " " ((++) (f_f_Geom2D_ptput a_f_Hs2__cent) 
        ((++) " " ((++) (f_f_StandardXfer_numtostr a_f_Hs2__surf) "\n")))));
    f_f_Hs2_puths ((F_T_Hs2_line a_f_Hs2__a a_f_Hs2__b a_f_Hs2__c),a_f_Hs2__surf)=(++) "li " ((++) (f_f_StandardXfer_realtostr a_f_Hs2__a) ((++) " " ((++) (f_f_StandardXfer_realtostr a_f_Hs2__b) 
        ((++) " " ((++) (f_f_StandardXfer_realtostr a_f_Hs2__c) ((++) " " ((++) (f_f_StandardXfer_numtostr a_f_Hs2__surf) "\n")))))));
    f_f_Hs2_putrop C_T_Hs2_Un='u';
    f_f_Hs2_putrop C_T_Hs2_Diff='d';
    f_f_Hs2_putrop C_T_Hs2_Inter='i';
type 
    T_t_main_SOLID=T_t_Csg_CSG T_t_Hs2_HS2 T_t_Hs2_ROP;
type 
    T_t_main_INTERVAL=(T_t_Misc_SCALAR,T_t_Misc_SCALAR);
type 
    T_t_main_CELL=(T_t_main_INTERVAL,T_t_main_INTERVAL);
type 
    T_t_main_SDSM=T_t_Gd_GDT (T_t_main_CELL,T_t_main_SOLID);
    c_f_main_UNIV=(((0.00000 :: Double),(1.00000 :: Double)),((0.00000 :: Double),(1.00000 :: Double)));
    f_f_main_prune_hs ((F_T_Hs2_circle a_f_main__srad a_f_main__scen),a_f_main__surfid) ((a_f_main__vxmin,a_f_main__vxmax),(a_f_main__vymin,a_f_main__vymax))=
        let { 
            r_f_main__vcen=f_f_Geom2D_ptmake (((*) :: (Double -> Double -> Double)) (((+) :: (Double -> Double -> Double)) a_f_main__vxmin a_f_main__vxmax) (0.500000 :: Double)) (((*) :: (Double -> Double -> Double)) (((+) :: (Double -> Double -> Double)) a_f_main__vymin a_f_main__vymax) (0.500000 :: Double));
            r_wC_area_0_=
                let { 
                    r_f_main__vrad=f_f_Geom2D_ptsdist r_f_main__vcen (f_f_Geom2D_ptmake a_f_main__vxmin a_f_main__vymin);
                    r_wC_area_1_=
                        let { 
                            r_f_main__distance=f_f_Geom2D_ptsdist a_f_main__scen r_f_main__vcen;
                            r_wC_area_2_=
                                let { 
                                    r_wC_area_3_=C_T_Csg_Fullsolid;
                                    r_wC_area_4_=
                                        let { 
                                            r_wC_area_5_=C_T_Csg_Emptysolid;
                                            r_wC_area_6_=F_T_Csg_Primitive (F_T_Hs2_circle a_f_main__srad a_f_main__scen,a_f_main__surfid)
                                         } in  
                                            if (((>=) :: (Double -> Double -> Bool)) r_f_main__distance (((+) :: (Double -> Double -> Double)) r_f_main__vrad a_f_main__srad))
                                            then r_wC_area_5_
                                            else 
                                                r_wC_area_6_
                                 } in  
                                    if (((<=) :: (Double -> Double -> Bool)) (((+) :: (Double -> Double -> Double)) r_f_main__distance r_f_main__vrad) a_f_main__srad)
                                    then r_wC_area_3_
                                    else 
                                        r_wC_area_4_
                         } in  f_id_THEPRIME r_wC_area_2_
                 } in  f_id_THEPRIME r_wC_area_1_
         } in  r_wC_area_0_;
    f_f_main_prune_hs ((F_T_Hs2_line a_f_main__a a_f_main__b a_f_main__c),a_f_main__surfid) ((a_f_main__vxmin,a_f_main__vxmax),(a_f_main__vymin,a_f_main__vymax))=
        let { 
            r_f_main__c0=((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_main__a a_f_main__vxmin) (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_main__b a_f_main__vymin) a_f_main__c);
            r_wC_area_7_=
                let { 
                    r_f_main__c1=((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_main__a a_f_main__vxmax) (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_main__b a_f_main__vymin) a_f_main__c);
                    r_wC_area_8_=
                        let { 
                            r_f_main__c2=((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_main__a a_f_main__vxmax) (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_main__b a_f_main__vymax) a_f_main__c);
                            r_wC_area_9_=
                                let { 
                                    r_f_main__c3=((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_main__a a_f_main__vxmin) (((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) a_f_main__b a_f_main__vymax) a_f_main__c);
                                    r_wC_area_10_=
                                        let { 
                                            r_wC_area_11_=C_T_Csg_Emptysolid;
                                            r_wC_area_12_=
                                                let { 
                                                    r_wC_area_13_=C_T_Csg_Fullsolid;
                                                    r_wC_area_14_=F_T_Csg_Primitive (F_T_Hs2_line a_f_main__a a_f_main__b a_f_main__c,a_f_main__surfid)
                                                 } in  
                                                    if (
                                                        if (
                                                            if (
                                                                if (((<=) :: (Double -> Double -> Bool)) r_f_main__c0 (0.00000 :: Double))
                                                                then (((<=) :: (Double -> Double -> Bool)) r_f_main__c1 (0.00000 :: Double))
                                                                else 
                                                                    False)
                                                            then (((<=) :: (Double -> Double -> Bool)) r_f_main__c2 (0.00000 :: Double))
                                                            else 
                                                                False)
                                                        then (((<=) :: (Double -> Double -> Bool)) r_f_main__c3 (0.00000 :: Double))
                                                        else 
                                                            False)
                                                    then r_wC_area_13_
                                                    else 
                                                        r_wC_area_14_
                                         } in  
                                            if (
                                                if (
                                                    if (
                                                        if (((>) :: (Double -> Double -> Bool)) r_f_main__c0 (0.00000 :: Double))
                                                        then (((>) :: (Double -> Double -> Bool)) r_f_main__c1 (0.00000 :: Double))
                                                        else 
                                                            False)
                                                    then (((>) :: (Double -> Double -> Bool)) r_f_main__c2 (0.00000 :: Double))
                                                    else 
                                                        False)
                                                then (((>) :: (Double -> Double -> Bool)) r_f_main__c3 (0.00000 :: Double))
                                                else 
                                                    False)
                                            then r_wC_area_11_
                                            else 
                                                r_wC_area_12_
                                 } in  f_id_THEPRIME r_wC_area_10_
                         } in  f_id_THEPRIME r_wC_area_9_
                 } in  f_id_THEPRIME r_wC_area_8_
         } in  r_wC_area_7_;
    f_f_main_merge_subtrees a_f_main__l C_T_Hs2_Un a_f_main__r=
        let { 
            r_wC_area_15_=f_id_THEPRIME a_f_main__l;
            r_wC_area_16_=
                let { 
                    r_wC_area_17_=f_id_THEPRIME a_f_main__r;
                    r_wC_area_18_=
                        let { 
                            r_wC_area_19_=f_id_THEPRIME a_f_main__r;
                            r_wC_area_20_=
                                let { 
                                    r_wC_area_21_=f_id_THEPRIME a_f_main__l;
                                    r_wC_area_22_=F_T_Csg_Compose a_f_main__l C_T_Hs2_Un a_f_main__r
                                 } in  
                                    if (f_f_Csg_is_empty a_f_main__r)
                                    then r_wC_area_21_
                                    else 
                                        r_wC_area_22_
                         } in  
                            if (f_f_Csg_is_empty a_f_main__l)
                            then r_wC_area_19_
                            else 
                                r_wC_area_20_
                 } in  
                    if (f_f_Csg_is_full a_f_main__r)
                    then r_wC_area_17_
                    else 
                        r_wC_area_18_
         } in  
            if (f_f_Csg_is_full a_f_main__l)
            then r_wC_area_15_
            else 
                r_wC_area_16_;
    f_f_main_merge_subtrees a_f_main__l C_T_Hs2_Inter a_f_main__r=
        let { 
            r_wC_area_23_=f_id_THEPRIME a_f_main__r;
            r_wC_area_24_=
                let { 
                    r_wC_area_25_=f_id_THEPRIME a_f_main__l;
                    r_wC_area_26_=
                        let { 
                            r_wC_area_27_=f_id_THEPRIME a_f_main__l;
                            r_wC_area_28_=
                                let { 
                                    r_wC_area_29_=f_id_THEPRIME a_f_main__r;
                                    r_wC_area_30_=F_T_Csg_Compose a_f_main__l C_T_Hs2_Inter a_f_main__r
                                 } in  
                                    if (f_f_Csg_is_empty a_f_main__r)
                                    then r_wC_area_29_
                                    else 
                                        r_wC_area_30_
                         } in  
                            if (f_f_Csg_is_empty a_f_main__l)
                            then r_wC_area_27_
                            else 
                                r_wC_area_28_
                 } in  
                    if (f_f_Csg_is_full a_f_main__r)
                    then r_wC_area_25_
                    else 
                        r_wC_area_26_
         } in  
            if (f_f_Csg_is_full a_f_main__l)
            then r_wC_area_23_
            else 
                r_wC_area_24_;
    f_f_main_merge_subtrees a_f_main__l C_T_Hs2_Diff a_f_main__r=
        let { 
            r_wC_area_31_=C_T_Csg_Emptysolid;
            r_wC_area_32_=
                let { 
                    r_wC_area_33_=C_T_Csg_Emptysolid;
                    r_wC_area_34_=
                        let { 
                            r_wC_area_35_=f_id_THEPRIME a_f_main__l;
                            r_wC_area_36_=F_T_Csg_Compose a_f_main__l C_T_Hs2_Diff a_f_main__r
                         } in  
                            if (f_f_Csg_is_empty a_f_main__r)
                            then r_wC_area_35_
                            else 
                                r_wC_area_36_
                 } in  
                    if (f_f_Csg_is_full a_f_main__r)
                    then r_wC_area_33_
                    else 
                        r_wC_area_34_
         } in  
            if (f_f_Csg_is_empty a_f_main__l)
            then r_wC_area_31_
            else 
                r_wC_area_32_;
    f_f_main_split_cell ((a_f_main__xmin,a_f_main__xmax),(a_f_main__ymin,a_f_main__ymax))=
        let { 
            r_f_main__x=((*) :: (Double -> Double -> Double)) (((+) :: (Double -> Double -> Double)) a_f_main__xmin a_f_main__xmax) (0.500000 :: Double);
            r_wC_area_37_=
                let { 
                    r_f_main__y=((*) :: (Double -> Double -> Double)) (((+) :: (Double -> Double -> Double)) a_f_main__ymin a_f_main__ymax) (0.500000 :: Double);
                    r_wC_area_38_=(:) ((a_f_main__xmin,r_f_main__x),(a_f_main__ymin,r_f_main__y)) ((:) (
                        (a_f_main__xmin,r_f_main__x),(r_f_main__y,a_f_main__ymax)) ((:) ((r_f_main__x,a_f_main__xmax),(a_f_main__ymin,r_f_main__y)) ((:) 
                        ((r_f_main__x,a_f_main__xmax),(r_f_main__y,a_f_main__ymax)) [])))
                 } in  f_id_THEPRIME r_wC_area_38_
         } in  r_wC_area_37_;
    f_f_main_make_subtrees [] a_wC_area_39_=[];
    f_f_main_make_subtrees (a_f_main__cell:a_f_main__rest) a_f_main__tree=(:) (a_f_main__cell,f_f_Csg_CSGtraverse_2 a_f_main__cell a_f_main__tree f_f_main_prune_hs f_f_main_merge_subtrees C_T_Csg_Fullsolid C_T_Csg_Emptysolid) (f_f_main_make_subtrees a_f_main__rest a_f_main__tree);
    f_f_main_simple (a_wC_area_40_,C_T_Csg_Fullsolid)=True;
    f_f_main_simple (a_wC_area_41_,C_T_Csg_Emptysolid)=True;
    f_f_main_simple (a_wC_area_42_,(F_T_Csg_Primitive a_wC_area_43_))=False;
    f_f_main_simple (a_wC_area_44_,(F_T_Csg_Compose a_wC_area_45_ a_wC_area_46_ a_wC_area_47_))=False;
    f_f_main_divide (a_f_main__cll,a_f_main__tree)=f_f_main_make_subtrees (f_f_main_split_cell a_f_main__cll) a_f_main__tree;
    c_f_main_IN=(0 :: Int);
    c_f_main_ON=(1 :: Int);
    c_f_main_OUT=(2 :: Int);
    c_f_main_DO_NOT_KNOW=(3 :: Int);
    c_f_main_UNION_TABLE=f_f_StandardVectors_listtovector ((:) c_f_main_IN ((:) c_f_main_IN ((:) c_f_main_IN ((:) c_f_main_IN ((:) c_f_main_IN 
        ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_ON ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_IN ((:) c_f_main_ON ((:) c_f_main_OUT ((:) c_f_main_DO_NOT_KNOW 
        ((:) c_f_main_IN ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_DO_NOT_KNOW []))))))))))))))));
    c_f_main_INTERSECT_TABLE=f_f_StandardVectors_listtovector ((:) c_f_main_IN ((:) c_f_main_ON ((:) c_f_main_OUT ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_ON 
        ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_OUT ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_OUT ((:) c_f_main_OUT ((:) c_f_main_OUT ((:) c_f_main_OUT 
        ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_OUT ((:) c_f_main_DO_NOT_KNOW []))))))))))))))));
    c_f_main_DIFF_TABLE=f_f_StandardVectors_listtovector ((:) c_f_main_OUT ((:) c_f_main_ON ((:) c_f_main_IN ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_OUT 
        ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_ON ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_OUT ((:) c_f_main_OUT ((:) c_f_main_OUT ((:) c_f_main_OUT 
        ((:) c_f_main_OUT ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_DO_NOT_KNOW ((:) c_f_main_DO_NOT_KNOW []))))))))))))))));
    f_f_main_classify_prim ((F_T_Hs2_circle a_f_main__r (F_T_Geom2D_Point2 a_f_main__cx a_f_main__cy)),a_wC_area_48_) (F_T_Geom2D_Point2 a_f_main__x a_f_main__y)=
        let { 
            r_f_main__dx=((-) :: (Double -> Double -> Double)) a_f_main__x a_f_main__cx;
            r_wC_area_49_=
                let { 
                    r_f_main__dy=((-) :: (Double -> Double -> Double)) a_f_main__y a_f_main__cy;
                    r_wC_area_50_=
                        let { 
                            r_f_main__d=((+) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) r_f_main__dx r_f_main__dx) (((-) :: (Double -> Double -> Double)) (((*) :: (Double -> Double -> Double)) r_f_main__dy r_f_main__dy) (((*) :: (Double -> Double -> Double)) a_f_main__r a_f_main__r));
                            r_wC_area_51_=
                                let { 
                                    r_wC_area_52_=f_id_THEPRIME c_f_main_OUT;
                                    r_wC_area_53_=
                                        let { 
                                            r_wC_area_54_=f_id_THEPRIME c_f_main_IN;
                                            r_wC_area_55_=f_id_THEPRIME c_f_main_ON
                                         } in  
                                            if (((<) :: (Double -> Double -> Bool)) r_f_main__d (0.00000 :: Double))
                                            then r_wC_area_54_
                                            else 
                                                r_wC_area_55_
                                 } in  
                                    if (((>) :: (Double -> Double -> Bool)) r_f_main__d (0.00000 :: Double))
                                    then r_wC_area_52_
                                    else 
                                        r_wC_area_53_
                         } in  f_id_THEPRIME r_wC_area_51_
                 } in  f_id_THEPRIME r_wC_area_50_
         } in  r_wC_area_49_;
    f_f_main_classify_prim ((F_T_Hs2_line a_f_main__a a_f_main__b a_f_main__c),a_wC_area_56_) (F_T_Geom2D_Point2 a_f_main__x a_f_main__y)=
        let { 
            r_f_main__d=((*) :: (Double -> Double -> Double)) a_f_main__a (((+) :: (Double -> Double -> Double)) a_f_main__x (((*) :: (Double -> Double -> Double)) a_f_main__b (((+) :: (Double -> Double -> Double)) a_f_main__y a_f_main__c)));
            r_wC_area_57_=
                let { 
                    r_wC_area_58_=f_id_THEPRIME c_f_main_OUT;
                    r_wC_area_59_=
                        let { 
                            r_wC_area_60_=f_id_THEPRIME c_f_main_IN;
                            r_wC_area_61_=f_id_THEPRIME c_f_main_ON
                         } in  
                            if (((<) :: (Double -> Double -> Bool)) r_f_main__d (0.00000 :: Double))
                            then r_wC_area_60_
                            else 
                                r_wC_area_61_
                 } in  
                    if (((>) :: (Double -> Double -> Bool)) r_f_main__d (0.00000 :: Double))
                    then r_wC_area_58_
                    else 
                        r_wC_area_59_
         } in  r_wC_area_57_;
    f_f_main_combine_classifications a_f_main__c1 C_T_Hs2_Un a_f_main__c2=f_f_std_index c_f_main_UNION_TABLE (((+) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) (((*) :: (Int -> Int -> Int)) (4 :: Int) a_f_main__c1) a_f_main__c2) (1 :: Int));
    f_f_main_combine_classifications a_f_main__c1 C_T_Hs2_Inter a_f_main__c2=f_f_std_index c_f_main_INTERSECT_TABLE (((+) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) (((*) :: (Int -> Int -> Int)) (4 :: Int) a_f_main__c1) a_f_main__c2) (1 :: Int));
    f_f_main_combine_classifications a_f_main__c1 C_T_Hs2_Diff a_f_main__c2=f_f_std_index c_f_main_DIFF_TABLE (((+) :: (Int -> Int -> Int)) (((+) :: (Int -> Int -> Int)) (((*) :: (Int -> Int -> Int)) (4 :: Int) a_f_main__c1) a_f_main__c2) (1 :: Int));
    f_f_main_approximate (((a_f_main__xmin,a_f_main__xmax),(a_f_main__ymin,a_f_main__ymax)),a_f_main__tree)=
        let { 
            r_f_main__midpt=f_f_Geom2D_ptmake (((*) :: (Double -> Double -> Double)) (((+) :: (Double -> Double -> Double)) a_f_main__xmin a_f_main__xmax) (0.500000 :: Double)) (((*) :: (Double -> Double -> Double)) (((+) :: (Double -> Double -> Double)) a_f_main__ymin a_f_main__ymax) (0.500000 :: Double));
            r_wC_area_62_=
                let { 
                    r_f_main__approx_tree=
                        let { 
                            r_wC_area_64_=C_T_Csg_Emptysolid;
                            r_wC_area_65_=C_T_Csg_Fullsolid
                         } in  
                            if (((==) :: (Int -> Int -> Bool)) (f_f_Csg_CSGtraverse_2 r_f_main__midpt a_f_main__tree f_f_main_classify_prim f_f_main_combine_classifications c_f_main_IN c_f_main_OUT) c_f_main_OUT)
                            then r_wC_area_64_
                            else 
                                r_wC_area_65_;
                    r_wC_area_63_=(((a_f_main__xmin,a_f_main__xmax),(a_f_main__ymin,a_f_main__ymax)),r_f_main__approx_tree)
                 } in  f_id_THEPRIME r_wC_area_63_
         } in  r_wC_area_62_;
    f_f_main_create_sdsm a_f_main__cell a_f_main__tree a_f_main__maxdepth=f_f_Gd_GDTcreate_1 (a_f_main__cell,a_f_main__tree) f_f_main_simple f_f_main_divide f_f_main_approximate a_f_main__maxdepth;
    f_f_main_quad_area (a_wC_area_66_,C_T_Csg_Emptysolid)=(0.00000 :: Double);
    f_f_main_quad_area (((a_f_main__xmin,a_f_main__xmax),a_wC_area_67_),C_T_Csg_Fullsolid)=
        let { 
            r_f_main__d=((-) :: (Double -> Double -> Double)) a_f_main__xmax a_f_main__xmin;
            r_wC_area_68_=((*) :: (Double -> Double -> Double)) r_f_main__d r_f_main__d
         } in  r_wC_area_68_;
    f_f_main_add_areas []=(0.00000 :: Double);
    f_f_main_add_areas (a_f_main__a:a_f_main__l)=((+) :: (Double -> Double -> Double)) a_f_main__a (f_f_main_add_areas a_f_main__l);
    f_f_main_area a_f_main__sdsm=f_f_Gd_GDTtraverse a_f_main__sdsm f_f_main_quad_area f_f_main_add_areas;
    f_f_main_test a_f_main__maxdepth=
        let { 
            (r_f_main__tree,r_wC_area_70_)=f_f_Csg_CSGget (f_f_StandardEnviron_fromfile "aap") f_f_Hs2_geths f_f_Hs2_getrop;
            r_wC_area_69_=
                let { 
                    r_f_main__sdsm=f_f_main_create_sdsm c_f_main_UNIV r_f_main__tree a_f_main__maxdepth;
                    r_wC_area_71_=f_f_StandardEnviron_termout ((++) (f_f_StandardXfer_realtostr (f_f_main_area r_f_main__sdsm)) "\n")
                 } in  f_id_THEPRIME r_wC_area_71_
         } in  r_wC_area_69_;
    f_abs::Double -> Double;
    f_abs a_x=
        if (((<=) :: (Double -> Double -> Bool)) a_x (0.00000 :: Double))
        then (((negate) :: (Double -> Double)) a_x)
        else 
            a_x;
    f_and::[Bool] -> Bool;
    f_and a_xs=f_foldr (&&) True a_xs;
    f_cjustify::Int -> [Char] -> [Char];
    f_cjustify a_n a_s=
        let { 
            r_margin=((-) :: (Int -> Int -> Int)) a_n (length a_s);
            r_lmargin=((quot) :: (Int -> Int -> Int)) r_margin (2 :: Int);
            r_rmargin=((-) :: (Int -> Int -> Int)) r_margin r_lmargin
         } in  (++) (f_spaces r_lmargin) ((++) a_s (f_spaces r_rmargin));
    f_concat::[[t1]] -> [t1];
    f_concat a_xs=f_foldr (++) [] a_xs;
    f_const::t1 -> t2 -> t1;
    f_const a_x a_y=a_x;
    f_digit::Char -> Bool;
    f_digit a_x=
        if (((<=) :: (Int -> Int -> Bool)) (fromEnum '0') (fromEnum a_x))
        then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_x) (fromEnum '9'))
        else 
            False;
    f_drop::Int -> [t1] -> [t1];
    f_drop 0 a_x=a_x;
    f_drop a_n (a_a:a_x)=f_drop (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x;
    f_drop a_n a_x=[];
    f_dropwhile::(t1 -> Bool) -> [t1] -> [t1];
    f_dropwhile a_f []=[];
    f_dropwhile a_f (a_a:a_x)=
        if (a_f a_a)
        then (f_dropwhile a_f a_x)
        else 
            ((:) a_a a_x);
    c_e::Double;
    c_e=((exp) :: (Double -> Double)) (1.00000 :: Double);
    f_filter::(t1 -> Bool) -> [t1] -> [t1];
    f_filter a_f a_x=[a_a|a_a<-a_x,a_f a_a];
    f_foldl::(t1 -> t2 -> t1) -> t1 -> [t2] -> t1;
    f_foldl a_op a_r []=a_r;
    f_foldl a_op a_r (a_a:a_x)=
        let { 
            f_strict a_f a_x=miraseq a_x (a_f a_x)
         } in  f_foldl a_op (f_strict a_op a_r a_a) a_x;
    f_foldl1::(t1 -> t1 -> t1) -> [t1] -> t1;
    f_foldl1 a_op (a_a:a_x)=f_foldl a_op a_a a_x;
    f_foldr::(t1 -> t2 -> t2) -> t2 -> [t1] -> t2;
    f_foldr a_op a_r []=a_r;
    f_foldr a_op a_r (a_a:a_x)=a_op a_a (f_foldr a_op a_r a_x);
    f_foldr1::(t1 -> t1 -> t1) -> [t1] -> t1;
    f_foldr1 a_op (a_a:[])=a_a;
    f_foldr1 a_op (a_a:a_b:a_x)=a_op a_a (f_foldr1 a_op ((:) a_b a_x));
    f_fst::(t1,t2) -> t1;
    f_fst (a_a,a_b)=a_a;
    f_id::t1 -> t1;
    f_id a_x=a_x;
    f_index::[t1] -> [Int];
    f_index a_x=
        let { 
            f_f a_n []=[];
            f_f a_n (a_a:a_x)=(:) a_n (f_f (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x)
         } in  f_f (0 :: Int) a_x;
    f_init::[t1] -> [t1];
    f_init (a_a:a_x)=
        if (null a_x)
        then []
        else 
            ((:) a_a (f_init a_x));
    f_iterate::(t1 -> t1) -> t1 -> [t1];
    f_iterate a_f a_x=(:) a_x (f_iterate a_f (a_f a_x));
    f_last::[t1] -> t1;
    f_last a_x=(!!) a_x (((-) :: (Int -> Int -> Int)) (length a_x) (1 :: Int));
    f_lay::[[Char]] -> [Char];
    f_lay []=[];
    f_lay (a_a:a_x)=(++) a_a ((++) "\n" (f_lay a_x));
    f_layn::[[Char]] -> [Char];
    f_layn a_x=
        let { 
            f_f a_n []=[];
            f_f a_n (a_a:a_x)=(++) (f_rjustify (4 :: Int) (strict_show_i a_n)) ((++) ") " ((++) a_a ((++) "\n" 
                (f_f (((+) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x))))
         } in  f_f (1 :: Int) a_x;
    f_letter::Char -> Bool;
    f_letter a_c=
        if (
            if (((<=) :: (Int -> Int -> Bool)) (fromEnum 'a') (fromEnum a_c))
            then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_c) (fromEnum 'z'))
            else 
                False)
        then True
        else 
        if (((<=) :: (Int -> Int -> Bool)) (fromEnum 'A') (fromEnum a_c))
        then (((<=) :: (Int -> Int -> Bool)) (fromEnum a_c) (fromEnum 'Z'))
        else 
            False;
    f_limit::[Double] -> Double;
    f_limit (a_a:a_b:a_x)=
        if (((==) :: (Double -> Double -> Bool)) a_a a_b)
        then a_a
        else 
            (f_limit ((:) a_b a_x));
    f_lines::[Char] -> [[Char]];
    f_lines []=[];
    f_lines (a_a:a_x)=
        let { 
            r_xs=
                if (pair a_x)
                then (f_lines a_x)
                else 
                    ((:) [] [])
         } in  
            if (((==) :: (Int -> Int -> Bool)) (fromEnum a_a) (fromEnum '\o012'))
            then ((:) [] (f_lines a_x))
            else 
                ((:) ((:) a_a (head r_xs)) (tail r_xs));
    f_ljustify::Int -> [Char] -> [Char];
    f_ljustify a_n a_s=(++) a_s (f_spaces (((-) :: (Int -> Int -> Int)) a_n (length a_s)));
    f_map::(t1 -> t2) -> [t1] -> [t2];
    f_map a_f a_x=[a_f a_a|a_a<-a_x];
    f_map2::(t1 -> t2 -> t3) -> [t1] -> [t2] -> [t3];
    f_map2 a_f a_x a_y=[a_f a_a a_b|(a_a,a_b)<-f_zip2 a_x a_y];
    f_max::[Int] -> Int;
    f_max a_xs=f_foldl1 f_max2 a_xs;
    f_max2::Int -> Int -> Int;
    f_max2 a_a a_b=
        if (((>=) :: (Int -> Int -> Bool)) a_a a_b)
        then a_a
        else 
            a_b;
    f_member::[Int] -> Int -> Bool;
    f_member a_x a_a=f_or (f_map (flip ((==) :: (Int -> Int -> Bool)) a_a) a_x);
    f_merge::[Int] -> [Int] -> [Int];
    f_merge [] a_y=a_y;
    f_merge (a_a:a_x) []=(:) a_a a_x;
    f_merge (a_a:a_x) (a_b:a_y)=
        if (((<=) :: (Int -> Int -> Bool)) a_a a_b)
        then ((:) a_a (f_merge a_x ((:) a_b a_y)))
        else 
            ((:) a_b (f_merge ((:) a_a a_x) a_y));
    f_min::[Int] -> Int;
    f_min a_xs=f_foldl1 f_min2 a_xs;
    f_min2::Int -> Int -> Int;
    f_min2 a_a a_b=
        if (((>) :: (Int -> Int -> Bool)) a_a a_b)
        then a_b
        else 
            a_a;
    f_mkset::[Int] -> [Int];
    f_mkset []=[];
    f_mkset (a_a:a_x)=(:) a_a (f_filter (flip ((/=) :: (Int -> Int -> Bool)) a_a) (f_mkset a_x));
    f_or::[Bool] -> Bool;
    f_or a_xs=f_foldr (||) False a_xs;
    c_pi::Double;
    c_pi=((*) :: (Double -> Double -> Double)) (4.00000 :: Double) (((atan) :: (Double -> Double)) (1.00000 :: Double));
    f_postfix::t1 -> [t1] -> [t1];
    f_postfix a_a a_x=(++) a_x ((:) a_a []);
    f_product::[Int] -> Int;
    f_product a_xs=f_foldl ((*) :: (Int -> Int -> Int)) (1 :: Int) a_xs;
    f_rep::Int -> t1 -> [t1];
    f_rep a_n a_x=f_take a_n (f_repeat a_x);
    f_repeat::t1 -> [t1];
    f_repeat a_x=(:) a_x (f_repeat a_x);
    f_reverse::[t1] -> [t1];
    f_reverse a_xs=f_foldl (flip (:)) [] a_xs;
    f_rjustify::Int -> [Char] -> [Char];
    f_rjustify a_n a_s=(++) (f_spaces (((-) :: (Int -> Int -> Int)) a_n (length a_s))) a_s;
    f_scan::(t1 -> t2 -> t1) -> t1 -> [t2] -> [t1];
    f_scan a_op=
        let { 
            f_g a_r []=(:) a_r [];
            f_g a_r (a_a:a_x)=(:) a_r (f_g (a_op a_r a_a) a_x)
         } in  f_g;
    f_snd::(t1,t2) -> t2;
    f_snd (a_a,a_b)=a_b;
    f_sort::[Int] -> [Int];
    f_sort a_x=
        let { 
            r_n=length a_x;
            r_n2=((quot) :: (Int -> Int -> Int)) r_n (2 :: Int)
         } in  
            if (((<=) :: (Int -> Int -> Bool)) r_n (1 :: Int))
            then a_x
            else 
                (f_merge (f_sort (f_take r_n2 a_x)) (f_sort (f_drop r_n2 a_x)));
    f_spaces::Int -> [Char];
    f_spaces a_n=f_rep a_n ' ';
    f_subtract::Int -> Int -> Int;
    f_subtract a_x a_y=((-) :: (Int -> Int -> Int)) a_y a_x;
    f_sum::[Int] -> Int;
    f_sum a_xs=f_foldl ((+) :: (Int -> Int -> Int)) (0 :: Int) a_xs;
data 
    T_sys_message=F_Stdout [Char] | F_Stderr [Char] | F_Tofile [Char] [Char] | F_Closefile [Char] | F_Appendfile [Char] | F_System [Char] | F_Exit Int;
    f_take::Int -> [t1] -> [t1];
    f_take 0 a_x=[];
    f_take a_n (a_a:a_x)=(:) a_a (f_take (((-) :: (Int -> Int -> Int)) a_n (1 :: Int)) a_x);
    f_take a_n a_x=[];
    f_takewhile::(t1 -> Bool) -> [t1] -> [t1];
    f_takewhile a_f []=[];
    f_takewhile a_f (a_a:a_x)=
        if (a_f a_a)
        then ((:) a_a (f_takewhile a_f a_x))
        else 
            [];
    f_transpose::[[t1]] -> [[t1]];
    f_transpose a_x=
        let { 
            r_x'=f_takewhile pair a_x
         } in  
            if (null r_x')
            then []
            else 
                ((:) (f_map head r_x') (f_transpose (f_map tail r_x')));
    f_until::(t1 -> Bool) -> (t1 -> t1) -> t1 -> t1;
    f_until a_f a_g a_x=
        if (a_f a_x)
        then a_x
        else 
            (f_until a_f a_g (a_g a_x));
    f_zip2::[t1] -> [t2] -> [(t1,t2)];
    f_zip2 (a_a:a_x) (a_b:a_y)=(:) (a_a,a_b) (f_zip2 a_x a_y);
    f_zip2 a_x a_y=[];
    f_zip3 (a_a:a_x) (a_b:a_y) (a_c:a_z)=(:) (a_a,a_b,a_c) (f_zip3 a_x a_y a_z);
    f_zip3 a_x a_y a_z=[];
    f_zip4 (a_a:a_w) (a_b:a_x) (a_c:a_y) (a_d:a_z)=(:) (a_a,a_b,a_c,a_d) (f_zip4 a_w a_x a_y a_z);
    f_zip4 a_w a_x a_y a_z=[];
    f_zip5 (a_a:a_v) (a_b:a_w) (a_c:a_x) (a_d:a_y) (a_e:a_z)=(:) (a_a,a_b,a_c,a_d,a_e) (f_zip5 a_v a_w a_x a_y a_z);
    f_zip5 a_v a_w a_x a_y a_z=[];
    f_zip6 (a_a:a_u) (a_b:a_v) (a_c:a_w) (a_d:a_x) (a_e:a_y) (a_f:a_z)=(:) (a_a,a_b,a_c,a_d,a_e,a_f) (f_zip6 a_u a_v a_w a_x a_y a_z);
    f_zip6 a_u a_v a_w a_x a_y a_z=[];
    f_zip::([t1],[t2]) -> [(t1,t2)];
    f_zip (a_x,a_y)=f_zip2 a_x a_y;
    f_main a_x=f_benchmark_main a_x;
    main = do (n:_) <- getArgs; putStr (f_main (read n :: Int))
}
