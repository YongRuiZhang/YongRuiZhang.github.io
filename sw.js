/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","19792cbc0e9952885eb234877ac276cc"],["/about/index.html","7636e61dc1bb8d02f87fad7849d3aae4"],["/archives/2023/01/index.html","12b03835f5976a2772d2e89c1d9c77a6"],["/archives/2023/02/index.html","a6d57d1d3a7cfd773d043a40738237b5"],["/archives/2023/02/page/2/index.html","529c8bdec629180e86a9ecdb60b4e604"],["/archives/2023/02/page/3/index.html","7d85fc22a1b860e1efb11387c194060c"],["/archives/2023/03/index.html","23b9835017fe533382e45fb0c3593cf5"],["/archives/2023/05/index.html","cefec59c1d0878f5e2f9201fc8dd879b"],["/archives/2023/06/index.html","2c12ca2cccc58f241047e44578a73e3b"],["/archives/2023/09/index.html","866174d8184fec6a6ac524dad118af86"],["/archives/2023/11/index.html","96bf43cec61d9fe546716530bbf89fa6"],["/archives/2023/12/index.html","ae37d7424438f3a592a909ab9feef6b2"],["/archives/2023/index.html","84a6a1f7d6d874565746d69bfb1d59f5"],["/archives/2023/page/2/index.html","0bb074d1bbc23e2f31cca0b434dced5c"],["/archives/2023/page/3/index.html","2089b8b0add1e02febcc8120ab86c267"],["/archives/2023/page/4/index.html","7d29b50b02216341ce3c5b1a1423abc2"],["/archives/2023/page/5/index.html","9e410c8c83ce3f4976483a7f99ead27f"],["/archives/2024/02/index.html","ce74e0743832e2dfeb4d39cd56e1fd9f"],["/archives/2024/03/index.html","4dc2f189a4659a54caf42880d02a1bf3"],["/archives/2024/index.html","3ae02946e1b5a0ed5e5091212265bd5c"],["/archives/index.html","2de93b3f4421b51f792080435c33f251"],["/archives/page/2/index.html","7535cd9299a66b80843ec285f2e47acb"],["/archives/page/3/index.html","74161030f76658ac86fbe1a66bae7042"],["/archives/page/4/index.html","f99463c396b088fa75e98ea80b380d3a"],["/archives/page/5/index.html","d1666ac0e1c13b29e80b474cb23e88ce"],["/baidu_verify_codeva-qQP2iZOMLX.html","4bb26fcaf574f5b3e28f2ab787ee4b76"],["/categories/Java/index.html","e39242696b1dc400644df7857341010a"],["/categories/Java/后端/index.html","d88c12ed7e7c112d04928cde096daa27"],["/categories/Java/基础/index.html","4054bfea726300fed09895ad812b93ae"],["/categories/Java/基础/集合/index.html","e1c28a1b5ffd883a28e70ba04d7adeb4"],["/categories/Python/index.html","966c0001b2557a75689b1e5847a4d16e"],["/categories/Python/编程环境/index.html","d8861004aba223554f4c8955090f881f"],["/categories/R语言/index.html","a229bc9444020d24a9bda6deff6329ae"],["/categories/R语言/编程环境/index.html","86a29631a3a6936fdda35a96fa5c0097"],["/categories/iPad/index.html","f61b4fcd371e77f9eb8270d07baf3e53"],["/categories/index.html","d2acf21d81147c083171ab0a6061d04b"],["/categories/中间件/index.html","4b80b9158644f782c5ab622e8ac67305"],["/categories/前端/Vue/index.html","fc815a10e54d9666d0755e680b632031"],["/categories/前端/index.html","a25a28acc9c16383a852e68a1acda902"],["/categories/大数据开发/ElasticSearch/index.html","6829ad0112036861f0bf1c6bac286da5"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","f112cfbd6e6421f7d907a472fe4b9b20"],["/categories/大数据开发/HBase/index.html","e2ade469525391557c196784621ed2a5"],["/categories/大数据开发/HBase/学习笔记/index.html","f539d7f5c9a6b363affafff058e2659f"],["/categories/大数据开发/HBase/环境搭建/index.html","5721808248f038ce1d74bfbaf0acd150"],["/categories/大数据开发/Hadoop/index.html","c7c901b12e86568fc220a6d0a6b3768d"],["/categories/大数据开发/Hadoop/技术/index.html","85bc3a68509c5b303e793705ac0b94cb"],["/categories/大数据开发/Hadoop/环境搭建/index.html","142e1e494d79747b5ac2d50ebd7ef29d"],["/categories/大数据开发/Redis/index.html","6d6f6391d3a0d169030af5a394f67a84"],["/categories/大数据开发/Redis/技术/index.html","90b518003e34971609451eec12f65195"],["/categories/大数据开发/Redis/环境搭建/index.html","053e5db3fde06bb534ea231e46c31232"],["/categories/大数据开发/Spark/index.html","b5cab899e9581cdaa836992fd762cb03"],["/categories/大数据开发/Spark/环境搭建/index.html","e05f4707011950292d5ae35631db2a8a"],["/categories/大数据开发/Zookeeper/index.html","2d28ddbfc9b4de17b572eea814bea8ae"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","1fbba419c6d407d1455ca46e703bb2a9"],["/categories/大数据开发/index.html","152f3cdef212bf77b584632488c7396b"],["/categories/学校课程/index.html","dc1d42b328a9b1c5c08f6f7a7f2e6344"],["/categories/学校课程/计算机操作系统/index.html","8b05ffa93266c5ab44b5886df91ca353"],["/categories/操作系统/Linux/index.html","67b1257b65737c851fc391012d9af7a6"],["/categories/操作系统/Mac/index.html","0ec9445dc496781c2ca4632db3bc5717"],["/categories/操作系统/Windows/index.html","d633cdd5857fafd194677ef72dbf53eb"],["/categories/操作系统/index.html","a246572cc8e6398f619bab64a8c9ad3f"],["/categories/数学建模/index.html","5c61c5f5804d1b952f80121c6b4d624a"],["/categories/数学建模/latex/index.html","a22d8454c54918fc521e88921aaf8a2b"],["/categories/数学建模/优化类/index.html","eb46ee06e4a8b3da89b5fa7d7b6aadce"],["/categories/数学建模/优化类/现代优化算法/index.html","aa68f95ca9a76a425f9f5c32ce86140f"],["/categories/数学建模/优化类/规划类/index.html","a184f29803b6f3820a45f8cfed443cd8"],["/categories/数学建模/绘图/index.html","2ed114bb3f64e230688dbe94dfe20623"],["/categories/数据库/MySQL/index.html","57ac06fb9a1a189e713a2b2bfc5c6970"],["/categories/数据库/index.html","bf0e7221b2a1c4980ada57ca6c056963"],["/categories/数据结构和算法/index.html","33dc30c5c680c11cfe0f078e38a4732c"],["/categories/数据结构和算法/page/2/index.html","fe680d4144f77679b37354af2b4dc2c9"],["/categories/数据结构和算法/基本原理/bfs/index.html","e77a7bfb36b9159773e5861602bae058"],["/categories/数据结构和算法/基本原理/dfs/index.html","5abffb5b1f92b34cfb4a4e50ec7445e0"],["/categories/数据结构和算法/基本原理/index.html","f610d92930914c18f5ba6156e7596c13"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","bbbc4cfeb1172c314ffc7196ab7a3794"],["/categories/数据结构和算法/基本原理/动态规划/index.html","810c0be42d21b8b297771a84ac0cc1d3"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","a60a3fa567942542b1219854c412d7ca"],["/categories/数据结构和算法/基本原理/图论/index.html","4b063897799b531361d717ae03d60408"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","53c4f05ffea09cadac255bfdfdf7c736"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","230107d8d5a8953de8cd4f0857794f37"],["/categories/数据结构和算法/基本原理/字符串/index.html","6ad54640fb8c69899f9d5df69c86a735"],["/categories/数据结构和算法/基本原理/排序/index.html","b6731855a56bcb14c069d6bf959f78a6"],["/categories/数据结构和算法/基本原理/搜索算法/index.html","a0fe1a1b460364ab8d6db6a5e64b125c"],["/categories/数据结构和算法/基本原理/搜索算法/宽度优先搜索算法/index.html","534a08d35fb1dcec4a8229a829493f05"],["/categories/数据结构和算法/基本原理/搜索算法/深度优先搜索算法/index.html","ad1d5622ad82156a21d0c606f9c64b00"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","e056b42862c1cb2f3103c36e85e5f7c8"],["/categories/数据结构和算法/基本原理/链表/index.html","b5dd99484f04d17a863246093cb6c3bc"],["/categories/数据结构和算法/算法题/index.html","b5de3ec10aac97661264a69cb7d2d623"],["/categories/数据结构和算法/算法题/二分查找/index.html","8a47172ddcbc5489fc13fbabc24d1393"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f127cfc227d567ccfde31e9c4cf54466"],["/categories/数据结构和算法/算法题/动态规划/index.html","8e796aa6f1234170e3b14aac151e5e55"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","05f7d9f42ad2da25334bdcd09b491228"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","69c6b6c9d16c7a26d1279dae05c59f1a"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","08faab626b9311d5e3d43e88a545e3b4"],["/categories/数据结构和算法/算法题/图论/index.html","ccdd6872ec6c2855b9e24be379c7d290"],["/categories/数据结构和算法/算法题/图论/树论/index.html","5656effdf8598ae5269ef62684a3584d"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","5fd0ebb8f94fa7c6c173f77c28cac0d3"],["/categories/数据结构和算法/算法题/数论/index.html","9c7c6f6301f703f3ce219e8f6c1e143d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","990963b6d3f9df29a93d8253ef2f7a9d"],["/categories/杂七杂八/index.html","143b56b50143130a08a50724328e1bbd"],["/categories/杂七杂八/博客搭建/index.html","a8979748ebf2400a9a560c707b88ca2d"],["/categories/编程工具下载/index.html","31287b7c07af53634a5aa4bf71e5e518"],["/categories/编程环境/index.html","cdf0dcfe45077a6cd5deeee6010b14cc"],["/categories/编程环境/大数据/index.html","a5017a8238d71f6f5b8fc7744af0e1d4"],["/categories/英语学习/index.html","839b451ad407127f078ad2208b38435c"],["/categories/英语学习/英语语法/index.html","2a5bfa4bfef47491e8e3c46b8349823f"],["/comments/index.html","52bb72507815d5e3d62713777cf74c7d"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","a111b0c0413178831ee84dfa61eff7ca"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","f1d2526b63627a9b2c77efc1e1bdfdc8"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","9c1076dec107afc7d870fbcdfd7adfee"],["/movies/index.html","7523f600faf16a84cb9c4021e2c8bc93"],["/music/index.html","3321afb9afa371f05367bd1240407a0a"],["/page/2/index.html","4f295b049a69dd14d3dca812007f5ba1"],["/page/3/index.html","423ed7f1fe9a9c9ed99be814aee77bae"],["/page/4/index.html","a75e989b8e56f3b1f0f16ea1303f70ee"],["/page/5/index.html","16ed9ecfbe2305f75aa6a1c7de129c78"],["/page/6/index.html","5a1e8eb333e4164e9df74a3f830175f0"],["/page/7/index.html","bf2d85580c95f768bb258a455f8d33fb"],["/posts/1021360842.html","a9603e089b0c1b8986496188797d341a"],["/posts/1120620192.html","3fb364e57cd09d44f14dac968cc30055"],["/posts/1137707673.html","c07fbcbbd035f56d8846c0db3f96dc31"],["/posts/1141628095.html","b157322b02d19d0887518f21b6cb55a2"],["/posts/1168613674.html","9e560efa9d7a76a7029c0e17bfb60d65"],["/posts/1219920510.html","9640c5c94b8e485aac2f830099d83f3c"],["/posts/1222166338.html","fe6fde8946b172ec4a2a2411a14e902b"],["/posts/1259097482.html","b1fa4f6f03ea57f3d0c9be9414bac48c"],["/posts/1271036369.html","163e8e4b3baddf2a55ad55c5bdc626d5"],["/posts/1312847445.html","aa7b0889c4e754d01f0bc015fd36e4c2"],["/posts/135355774.html","ec68bf2481a38a276f8e86ff0bc43f5c"],["/posts/1375344716.html","047cda2d49702f5d9cb4b724861af8ae"],["/posts/1388991698.html","334a322be1667c318b2697fa4a97501e"],["/posts/1410315814.html","ae632d2985d75093983be99682998aec"],["/posts/1452790229.html","a47f513e4fa492278e3dda2c8dadb46c"],["/posts/1470079884.html","c4db1db3cad8480d1fda458bc9f5e1a2"],["/posts/1470079885.html","2fe30905fd2b6e69beabe2d5bd85702a"],["/posts/1470079886.html","1c5ba7c3e90e3dfebcae2d78c2a20006"],["/posts/1470079887.html","1520f2068dfe4e3404f562e10a0d9e72"],["/posts/1498536549.html","fb7e082a0393830453388d65c22c84e2"],["/posts/1539568593.html","812c29e0b5626ac8ee0202e3bf8d53dc"],["/posts/1547067935.html","9a3a5b2f5a8c348706524e87884d818d"],["/posts/1557866301.html","7d42cbcebaa2cca2bb9716c75d75531f"],["/posts/1571776361.html","453bdfa09afa7db738666617d02415e9"],["/posts/1605124548.html","3dc7de52966ff86a2a6aacb6db381fb2"],["/posts/1633036852.html","9e27424e3b32ee72bcfcdaac827dc0f0"],["/posts/1667740714.html","7c1decb8564b91c812984edf7c98d48e"],["/posts/1674202625.html","80423b0c646cc7b8f120c7062b8dffff"],["/posts/1765123828.html","068b6798c1084af9a8b8152bff19d784"],["/posts/1767336200.html","b83af2bc9e0a4f439b62d1cdfb8bd6e6"],["/posts/1776114197.html","bcd3389970530bb5d5cbcc5b22cf6ae4"],["/posts/1817748743.html","847e68a2e4480d5c700aff515f8f3bdb"],["/posts/1925125395.html","5ba0a6875a5066c9751b7b0c22c48ad8"],["/posts/1966191251.html","2f63f67d94131eb00ac0bae1f2ffa88f"],["/posts/1987617322.html","83052dcf661693ea387fdcad4fbbbd3c"],["/posts/1999788039.html","9ec2af91e3793d43123e93f9e6a98fd6"],["/posts/2007534187.html","94703bfdcb25c8ae313e33b560d04992"],["/posts/2075104059.html","99a61ee5a25390ab7a7180c9b99ad130"],["/posts/2087796737.html","5e79be8ad892b6555fc011e36494c1a4"],["/posts/2106547339.html","ee73ff6297aa3c6faf5748207dcdebeb"],["/posts/2207806286.html","66d085be2efa869394ae3980df0df85e"],["/posts/2225903441.html","eaf564f75b724b2c0989b94bf677562b"],["/posts/2265610284.html","fb8af95f2f72f8777d7312d929f89cd3"],["/posts/2281352001.html","cf0b54498d9ff8afaaf23c1ba9848e3e"],["/posts/2364755265.html","ff3c0bc9e83618c3fcae1b16b551a7f1"],["/posts/2414116852.html","5d92ef5b953b084a901e133a18f7f056"],["/posts/2421785022.html","12b330dee6e3467ce8bb4513e9d5a884"],["/posts/2482902029.html","7d1486f40a012ac993b129c8b10d5e75"],["/posts/2495386210.html","a04ba84e335baec039a3099744ceb92b"],["/posts/2516528882.html","691dec5ea060962289fb32dacc607bda"],["/posts/2522177458.html","fdcf94d758b1c72d7db7a133d3ce0240"],["/posts/2526659543.html","71242ed228ecb3bf8d4bc26233dfd497"],["/posts/2529807823.html","a6bd196e8104be6175fb29066df0b1ab"],["/posts/2592249117.html","2634f5c73cffa0eb692a364bc87ed390"],["/posts/2596601004.html","4cc3a105a8545b8c8986b44e6491d595"],["/posts/2697614349.html","a690251654b40890be7b4b3e57eb6029"],["/posts/2742438348.html","5f7eac4e120466bc9012533a13ae67d5"],["/posts/2768249503.html","b3664eed1ac2dd323123e5ada8500414"],["/posts/2864584994.html","bfd1b55637e408baa40d451ff927615a"],["/posts/2888309600.html","504c543509749a181bac345829424c5d"],["/posts/2891591958.html","d33ea443c93aa1e0ca05624ff6d7f5d4"],["/posts/2909934084.html","2259e5623b7ad98ee38dd119c9bbee00"],["/posts/2920256992.html","0e49a83d6adef32fa8793768c1b6c790"],["/posts/2959474469.html","da25dc535b085e9690d0c4adb62722b8"],["/posts/3005926051.html","b7290a4816043ed7b324fd6d8a81263f"],["/posts/309775400.html","aa626869cca5174dbda123ad34e92c48"],["/posts/3156194925.html","40d1a8a97238e69c8d9c9ddeea265bcb"],["/posts/3169224211.html","c8d081ccc49309fd166440827df6a17f"],["/posts/3183912587.html","0249e0b1bdda0db2977a44ebca731f59"],["/posts/3213899550.html","0860eea8443fd17e57a051d81fee53c7"],["/posts/3259212833.html","9cfcb98dd59b580437a9a4660d381a8a"],["/posts/3265658309.html","30e86709b4dc8c5e43f13f301d5da2be"],["/posts/3266130344.html","0a5752b3846cf89ccbc0c6b447003e08"],["/posts/3292663995.html","32277b4abba6e39940e9260fda79ba81"],["/posts/3297135020.html","eedcd0f895a6a229a0c289d486a1f195"],["/posts/3306641566.html","8911e04ffbe8c6d7aa341d94c34df994"],["/posts/3312011324.html","0198c1107949303fbc19d1e7dd90b289"],["/posts/336911618.html","4a78a82fd2aad02b931a6edca46f773c"],["/posts/3402121571.html","63f83550e5b399baf6090b9c871be991"],["/posts/3405577485.html","67447994ce48ef7e06d259896439cda2"],["/posts/3498516849.html","3206c7fdf0769783cc161c36f9f00cc8"],["/posts/350679531.html","4e2822fd300c17165779283521b0a599"],["/posts/3513711414.html","27fdc54e4b2a749abdc669acb3622140"],["/posts/3523095624.html","a08f3aa6a25e3444b82ae8e1ec8dfdf5"],["/posts/3546711884.html","e86031ab1d93ad6b17be6cc85ba1ad19"],["/posts/362397694.html","776a4038153e723bf0d9427a8bf78337"],["/posts/3731385230.html","7fbf29b7edf48b8b8f12e054a6679520"],["/posts/3772089482.html","cddd907048372a9d2b59d7576e7c0a8b"],["/posts/386609427.html","31d50123afd14ae94f5e80d2954a6cbc"],["/posts/4044235327.html","fba2cd67e8a5b0506ee125a84c04dce7"],["/posts/4098221856.html","f891681251bdb130f6b4c269725d9c2a"],["/posts/4115971639.html","cbd4056f38e88c1e95b683b5608458d6"],["/posts/4130790367.html","eda46db9b9a3769304bf265a60426c08"],["/posts/4131986683.html","8549583ca1b52e35f5dfa419ceaee606"],["/posts/4177218757.html","a57c05f8a07695626a6035917b9bff5e"],["/posts/4192183953.html","1ef290ecdc4a670ec6a4878e4b81b70a"],["/posts/4223662913.html","3e66dba65944e51b13038791bc61def1"],["/posts/4261103898.html","7ff238e96627b83a9760305461a649b3"],["/posts/4286605504.html","91f205dd558f9a83b0e12d7135569aa0"],["/posts/449089913.html","ba5510929231cc0999a955212ffdf1fd"],["/posts/469277133.html","c1b71432e5a320a3db6573005a5cdc3c"],["/posts/469711973.html","8d5be0c42b8dcf61f900394dd6a02b99"],["/posts/482495853.html","cf7a82ce40704d5db9e5fe0ab8767d69"],["/posts/488247922.html","3f71cce0c9eb0f999c417b64542cc556"],["/posts/517302816.html","88ec910544d7c690513adadb950e90bc"],["/posts/570165348.html","1332dad64dac8cd276ff73c553451d92"],["/posts/595890772.html","9e27d08f7afcbbf8e355da52f1ab1811"],["/posts/67485572.html","c45628b2bdfedcbc8e3a1ee5bb8ed1c1"],["/posts/694347442.html","42a1b56eae727015e94219eb0d411a2a"],["/posts/707384687.html","6e9cf12348a0378f0b589d5b41dc43ec"],["/posts/71180092.html","f130b6e8058da3dafc3c44130b4cfa3c"],["/posts/716459272.html","4d75556545916ce99a4c46a6ad08036f"],["/posts/765481613.html","316fb639553f686c734023bf3d98d9fc"],["/posts/778231993.html","ebf474bbb9b13df53b4e59f432374ee6"],["/posts/795397410.html","bcb260b3a1a2ad08d4e34ae3bce56bae"],["/posts/820223701.html","e6e579c89bd279dbced74493eef19c6f"],["/posts/830372185.html","de9337b7e8235be9d05ad5755a8c6e36"],["/posts/88294277.html","893079ad8829f49b882af3ae1ed90306"],["/posts/939963535.html","8879be734205ca83c1cc1a6db5390d74"],["/posts/983786067.html","bf874c65a7560d88b2e1beaa484ae8da"],["/sw-register.js","54857081ffd3bfa6aa82339f2ae8d408"],["/tags/C/index.html","276fe5b009661852a5b7bf06b362ae53"],["/tags/C/page/2/index.html","509b655fba91cae98624bd8f33fb4883"],["/tags/C/page/3/index.html","7f16b22d9fdc559389009c28451bd997"],["/tags/C/page/4/index.html","ef9e7b8de311cfe236198688bf2bc994"],["/tags/ETL/index.html","d1646abf6a16b6bbbcacbbc33574b601"],["/tags/ElasticSearch/index.html","5635f09d1d87105945948f866adb3ff9"],["/tags/GUI/index.html","c88c47e28db690cb937409b24e00f75f"],["/tags/HBase/index.html","1a1a15a58939a0c8df59d3128d43fe00"],["/tags/Hadoop/index.html","383944a6e631528d0451fea03f705b58"],["/tags/Hadoop/page/2/index.html","8a034c499c735c8d3b035dcdab5f4000"],["/tags/Java/index.html","057109120400b8ab43cf3228933322a2"],["/tags/Java/page/2/index.html","b2ee9221406a4ae06e5ac4c383b12221"],["/tags/Java后端/index.html","d12edfc76be1fca8efcf6875399fd7e6"],["/tags/Java后端/page/2/index.html","5b097248823ad14d6259911f0a9ccb72"],["/tags/Kettle/index.html","c43ceeb6390be19ba9f68a335d3253bc"],["/tags/Kibana/index.html","5302246f28a4438740f04d583b61c339"],["/tags/Linux/index.html","1c52004a50bf5f88d474d105ee3b7844"],["/tags/Linux/page/2/index.html","e12638b993ae967d014e30dea30e564c"],["/tags/Linux/page/3/index.html","cd955d877a68ced19cef811b597bcc5a"],["/tags/Mac/index.html","1b9764a76c32ffce25d3772147519e97"],["/tags/Mac/page/2/index.html","3c73584d27a49d494002af0fbe940d93"],["/tags/Maven/index.html","3f632f6bb33be83e5f423295fe9433c8"],["/tags/MySQL/index.html","b6092bd577fa66a1aa59c4925713983c"],["/tags/Python/index.html","d1f5de0c37dc5e21395ada19f7cb987f"],["/tags/Redis/index.html","96c6de0bf2529738b5b6c548ef2fc61f"],["/tags/R语言/index.html","3c2df0cbecbc9a1f6bf9cb077d8ba971"],["/tags/Spark/index.html","c0aa867d8623dd1dd9904715cf803b2c"],["/tags/Ubuntu/index.html","af288e4c6adecca3327d98fe7f152a4c"],["/tags/Vue/index.html","b6a643162824c9d43d906eabc65ac6e4"],["/tags/Windows/index.html","ad74c864480841aa46d52a84bd4cf1a4"],["/tags/ZooKeeper/index.html","8331268bc26038f0e380087e4231b4ef"],["/tags/bfs/index.html","f79a4a626c5afb1c305ad12040b4a5cb"],["/tags/dfs/index.html","d1877a2e6a0117f313bb680ac1843e32"],["/tags/folium/index.html","a762b2cbca00ebf9a9eefedf01ae9820"],["/tags/git/index.html","9cb17b49306228d36a3e1487c50100eb"],["/tags/iPad找电子书/index.html","7ddc442e0d66d966044e32f84a1c6aa9"],["/tags/index.html","5b05aa70358d16c0160406d1b84c96c0"],["/tags/latex/index.html","43a337e94bea9ecf9f86849b15b47a02"],["/tags/中间件/index.html","982915f4dba873682a9c7a2b001a3f52"],["/tags/二分查找/index.html","fc9e06a226c6db0d20f61dfa4ca86019"],["/tags/优化类/index.html","39de451dc641d1ee68ac6613c4437b13"],["/tags/前端/index.html","e345841732da3bcc93701b61352b7fa8"],["/tags/前缀和与差分/index.html","263e1d48551028f571cb9da240e41c5b"],["/tags/动态规划/index.html","b7de31258f2544065ab1f14bde0f5381"],["/tags/动态规划/page/2/index.html","136d0f76a49de55502009ebb9b2606e0"],["/tags/博客搭建/index.html","4bff01dd46f3c50d3206946b7d54ffe9"],["/tags/图论/index.html","d646c5ffe687401230f38776650424c3"],["/tags/图论/page/2/index.html","69fa60812c153ebb6fdcecc94519d57b"],["/tags/大数据/index.html","f1b1a6e615b7557e583079475df11f39"],["/tags/大数据/page/2/index.html","6c3608b978d3c910ad56cdaf37181131"],["/tags/宽度优先搜索算法/index.html","7371f3006a8030375f4f54e2ff906cee"],["/tags/排序/index.html","3e3ecfff97de2423c65fed20c50ee336"],["/tags/操作系统/index.html","92f496b591f577f1f16ad66b35ffbf51"],["/tags/数学建模/index.html","816d4816b514bf4a765248b867b003de"],["/tags/数据库/index.html","39d4bc8aec5a2da538805047e9f7cfc6"],["/tags/数据结构和算法/index.html","83455aa59717c39c6aee1843a1d86a4c"],["/tags/数据结构和算法/page/2/index.html","3d745eb0654d5abf46294c6c64d7ceba"],["/tags/数据结构和算法/page/3/index.html","dce361489a8cb8a6d9135f116be897cc"],["/tags/数据结构和算法/page/4/index.html","175968ffa8dfa4fdd83fab6c7ef488aa"],["/tags/数据结构和算法/page/5/index.html","f4bb3673ec5a96b61b21f481bef301ea"],["/tags/数组和字符串/index.html","be1db29540e130d91063acd6ff6a7a23"],["/tags/数论/index.html","c26ccbeab6468a92d732b1d7ce39de2b"],["/tags/枚举类/index.html","3da08bf1e9cc2f5eeaeaf022a693eaa9"],["/tags/栈和队列/index.html","08064c2c206d05620bacd6159ec02b39"],["/tags/树论/index.html","999df264738190978314d269b8748204"],["/tags/测试/index.html","b9e01b03ac2ba2a3cbd78552f30aa1c1"],["/tags/深度优先搜索算法/index.html","117010643c65b7508450d05b23cfc7bb"],["/tags/环境/index.html","babb69b53407db0d5edf9aca2685803e"],["/tags/环境变量/index.html","c45cb7a2482ba3c76247ef347f3ad898"],["/tags/绘图/index.html","5862acd9a89701cc08f9780d77fdb279"],["/tags/编程工具/index.html","65940e2b658fb97b522242f65cbdc631"],["/tags/编程环境/index.html","de0c5f2f13be60c3e737c499dfc71fa0"],["/tags/网络编程/index.html","d86c90f26c10a6ae92dbea483c4b2182"],["/tags/英语语法/index.html","4c0b0cfd744362c7b49ff8797d4050dc"],["/tags/计算机操作系统/index.html","b10bfb730d4e96b33cf7e70cdf715c5e"],["/tags/论文/index.html","a2aed838d2aaca8ca4d0641b01ce2f5f"],["/tags/资源下载/index.html","5ee84f90b1c2b41144cfd491069fd310"],["/tags/链表/index.html","799def873511e6574bf2cdbd463af5ab"],["/tags/集合/index.html","a7b0c8812f62dbba712ca9b3654487a0"],["/tags/集群/index.html","ca6a9a57ab702b920f377dc3deda56b4"]];
var cacheName = 'sw-precache-v3--' + (self.registration ? self.registration.scope : '');
var firstRegister = 1; // 默认1是首次安装SW， 0是SW更新


var ignoreUrlParametersMatching = [/^utm_/];


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var cleanResponse = function (originalResponse) {
    // 如果没有重定向响应，不需干啥
    if (!originalResponse.redirected) {
        return Promise.resolve(originalResponse);
    }

    // Firefox 50 及以下不知处 Response.body 流, 所以我们需要读取整个body以blob形式返回。
    var bodyPromise = 'body' in originalResponse ?
        Promise.resolve(originalResponse.body) :
        originalResponse.blob();

    return bodyPromise.then(function (body) {
        // new Response() 可同时支持 stream or Blob.
        return new Response(body, {
            headers: originalResponse.headers,
            status: originalResponse.status,
            statusText: originalResponse.statusText
        });
    });
};

var createCacheKey = function (originalUrl, paramName, paramValue,
    dontCacheBustUrlsMatching) {

    // 创建一个新的URL对象，避免影响原始URL
    var url = new URL(originalUrl);

    // 如果 dontCacheBustUrlsMatching 值没有设置，或是没有匹配到，将值拼接到url.serach后
    if (!dontCacheBustUrlsMatching ||
        !(url.pathname.match(dontCacheBustUrlsMatching))) {
        url.search += (url.search ? '&' : '') +
            encodeURIComponent(paramName) + '=' + encodeURIComponent(paramValue);
    }

    return url.toString();
};

var isPathWhitelisted = function (whitelist, absoluteUrlString) {
    // 如果 whitelist 是空数组，则认为全部都在白名单内
    if (whitelist.length === 0) {
        return true;
    }

    // 否则逐个匹配正则匹配并返回
    var path = (new URL(absoluteUrlString)).pathname;
    return whitelist.some(function (whitelistedPathRegex) {
        return path.match(whitelistedPathRegex);
    });
};

var stripIgnoredUrlParameters = function (originalUrl,
    ignoreUrlParametersMatching) {
    var url = new URL(originalUrl);
    // 移除 hash; 查看 https://github.com/GoogleChrome/sw-precache/issues/290
    url.hash = '';

    url.search = url.search.slice(1) // 是否包含 '?'
        .split('&') // 分割成数组 'key=value' 的形式
        .map(function (kv) {
            return kv.split('='); // 分割每个 'key=value' 字符串成 [key, value] 形式
        })
        .filter(function (kv) {
            return ignoreUrlParametersMatching.every(function (ignoredRegex) {
                return !ignoredRegex.test(kv[0]); // 如果 key 没有匹配到任何忽略参数正则，就 Return true
            });
        })
        .map(function (kv) {
            return kv.join('='); // 重新把 [key, value] 格式转换为 'key=value' 字符串
        })
        .join('&'); // 将所有参数 'key=value' 以 '&' 拼接

    return url.toString();
};


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var hashParamName = '_sw-precache';
var urlsToCacheKeys = new Map(
    precacheConfig.map(function (item) {
        var relativeUrl = item[0];
        var hash = item[1];
        var absoluteUrl = new URL(relativeUrl, self.location);
        var cacheKey = createCacheKey(absoluteUrl, hashParamName, hash, false);
        return [absoluteUrl.toString(), cacheKey];
    })
);

function setOfCachedUrls(cache) {
    return cache.keys().then(function (requests) {
        // 如果原cacheName中没有缓存任何收，就默认是首次安装，否则认为是SW更新
        if (requests && requests.length > 0) {
            firstRegister = 0; // SW更新
        }
        return requests.map(function (request) {
            return request.url;
        });
    }).then(function (urls) {
        return new Set(urls);
    });
}

self.addEventListener('install', function (event) {
    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return setOfCachedUrls(cache).then(function (cachedUrls) {
                return Promise.all(
                    Array.from(urlsToCacheKeys.values()).map(function (cacheKey) {
                        // 如果缓存中没有匹配到cacheKey，添加进去
                        if (!cachedUrls.has(cacheKey)) {
                            var request = new Request(cacheKey, { credentials: 'same-origin' });
                            return fetch(request).then(function (response) {
                                // 只要返回200才能继续，否则直接抛错
                                if (!response.ok) {
                                    throw new Error('Request for ' + cacheKey + ' returned a ' +
                                        'response with status ' + response.status);
                                }

                                return cleanResponse(response).then(function (responseToCache) {
                                    return cache.put(cacheKey, responseToCache);
                                });
                            });
                        }
                    })
                );
            });
        })
            .then(function () {
            
            // 强制 SW 状态 installing -> activate
            return self.skipWaiting();
            
        })
    );
});

self.addEventListener('activate', function (event) {
    var setOfExpectedUrls = new Set(urlsToCacheKeys.values());

    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return cache.keys().then(function (existingRequests) {
                return Promise.all(
                    existingRequests.map(function (existingRequest) {
                        // 删除原缓存中相同键值内容
                        if (!setOfExpectedUrls.has(existingRequest.url)) {
                            return cache.delete(existingRequest);
                        }
                    })
                );
            });
        }).then(function () {
            
            return self.clients.claim();
            
        }).then(function () {
                // 如果是首次安装 SW 时, 不发送更新消息（是否是首次安装，通过指定cacheName 中是否有缓存信息判断）
                // 如果不是首次安装，则是内容有更新，需要通知页面重载更新
                if (!firstRegister) {
                    return self.clients.matchAll()
                        .then(function (clients) {
                            if (clients && clients.length) {
                                clients.forEach(function (client) {
                                    client.postMessage('sw.update');
                                })
                            }
                        })
                }
            })
    );
});



    self.addEventListener('fetch', function (event) {
        if (event.request.method === 'GET') {

            // 是否应该 event.respondWith()，需要我们逐步的判断
            // 而且也方便了后期做特殊的特殊
            var shouldRespond;


            // 首先去除已配置的忽略参数及hash
            // 查看缓存简直中是否包含该请求，包含就将shouldRespond 设为true
            var url = stripIgnoredUrlParameters(event.request.url, ignoreUrlParametersMatching);
            shouldRespond = urlsToCacheKeys.has(url);

            // 如果 shouldRespond 是 false, 我们在url后默认增加 'index.html'
            // (或者是你在配置文件中自行配置的 directoryIndex 参数值)，继续查找缓存列表
            var directoryIndex = 'index.html';
            if (!shouldRespond && directoryIndex) {
                url = addDirectoryIndex(url, directoryIndex);
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 仍是 false，检查是否是navigation
            // request， 如果是的话，判断是否能与 navigateFallbackWhitelist 正则列表匹配
            var navigateFallback = '';
            if (!shouldRespond &&
                navigateFallback &&
                (event.request.mode === 'navigate') &&
                isPathWhitelisted([], event.request.url)
            ) {
                url = new URL(navigateFallback, self.location).toString();
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 被置为 true
            // 则 event.respondWith()匹配缓存返回结果，匹配不成就直接请求.
            if (shouldRespond) {
                event.respondWith(
                    caches.open(cacheName).then(function (cache) {
                        return cache.match(urlsToCacheKeys.get(url)).then(function (response) {
                            if (response) {
                                return response;
                            }
                            throw Error('The cached response that was expected is missing.');
                        });
                    }).catch(function (e) {
                        // 如果捕获到异常错误，直接返回 fetch() 请求资源
                        console.warn('Couldn\'t serve response for "%s" from cache: %O', event.request.url, e);
                        return fetch(event.request);
                    })
                );
            }
        }
    });



// *** Start of auto-included sw-toolbox code. ***
/* 
 Copyright 2016 Google Inc. All Rights Reserved.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/!function(e){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=e();else if("function"==typeof define&&define.amd)define([],e);else{var t;t="undefined"!=typeof window?window:"undefined"!=typeof global?global:"undefined"!=typeof self?self:this,t.toolbox=e()}}(function(){return function e(t,n,r){function o(c,s){if(!n[c]){if(!t[c]){var a="function"==typeof require&&require;if(!s&&a)return a(c,!0);if(i)return i(c,!0);var u=new Error("Cannot find module '"+c+"'");throw u.code="MODULE_NOT_FOUND",u}var f=n[c]={exports:{}};t[c][0].call(f.exports,function(e){var n=t[c][1][e];return o(n?n:e)},f,f.exports,e,t,n,r)}return n[c].exports}for(var i="function"==typeof require&&require,c=0;c<r.length;c++)o(r[c]);return o}({1:[function(e,t,n){"use strict";function r(e,t){t=t||{};var n=t.debug||m.debug;n&&console.log("[sw-toolbox] "+e)}function o(e){var t;return e&&e.cache&&(t=e.cache.name),t=t||m.cache.name,caches.open(t)}function i(e,t){t=t||{};var n=t.successResponses||m.successResponses;return fetch(e.clone()).then(function(r){return"GET"===e.method&&n.test(r.status)&&o(t).then(function(n){n.put(e,r).then(function(){var r=t.cache||m.cache;(r.maxEntries||r.maxAgeSeconds)&&r.name&&c(e,n,r)})}),r.clone()})}function c(e,t,n){var r=s.bind(null,e,t,n);d=d?d.then(r):r()}function s(e,t,n){var o=e.url,i=n.maxAgeSeconds,c=n.maxEntries,s=n.name,a=Date.now();return r("Updating LRU order for "+o+". Max entries is "+c+", max age is "+i),g.getDb(s).then(function(e){return g.setTimestampForUrl(e,o,a)}).then(function(e){return g.expireEntries(e,c,i,a)}).then(function(e){r("Successfully updated IDB.");var n=e.map(function(e){return t.delete(e)});return Promise.all(n).then(function(){r("Done with cache cleanup.")})}).catch(function(e){r(e)})}function a(e,t,n){return r("Renaming cache: ["+e+"] to ["+t+"]",n),caches.delete(t).then(function(){return Promise.all([caches.open(e),caches.open(t)]).then(function(t){var n=t[0],r=t[1];return n.keys().then(function(e){return Promise.all(e.map(function(e){return n.match(e).then(function(t){return r.put(e,t)})}))}).then(function(){return caches.delete(e)})})})}function u(e,t){return o(t).then(function(t){return t.add(e)})}function f(e,t){return o(t).then(function(t){return t.delete(e)})}function h(e){e instanceof Promise||p(e),m.preCacheItems=m.preCacheItems.concat(e)}function p(e){var t=Array.isArray(e);if(t&&e.forEach(function(e){"string"==typeof e||e instanceof Request||(t=!1)}),!t)throw new TypeError("The precache method expects either an array of strings and/or Requests or a Promise that resolves to an array of strings and/or Requests.");return e}function l(e,t,n){if(!e)return!1;if(t){var r=e.headers.get("date");if(r){var o=new Date(r);if(o.getTime()+1e3*t<n)return!1}}return!0}var d,m=e("./options"),g=e("./idb-cache-expiration");t.exports={debug:r,fetchAndCache:i,openCache:o,renameCache:a,cache:u,uncache:f,precache:h,validatePrecacheInput:p,isResponseFresh:l}},{"./idb-cache-expiration":2,"./options":4}],2:[function(e,t,n){"use strict";function r(e){return new Promise(function(t,n){var r=indexedDB.open(u+e,f);r.onupgradeneeded=function(){var e=r.result.createObjectStore(h,{keyPath:p});e.createIndex(l,l,{unique:!1})},r.onsuccess=function(){t(r.result)},r.onerror=function(){n(r.error)}})}function o(e){return e in d||(d[e]=r(e)),d[e]}function i(e,t,n){return new Promise(function(r,o){var i=e.transaction(h,"readwrite"),c=i.objectStore(h);c.put({url:t,timestamp:n}),i.oncomplete=function(){r(e)},i.onabort=function(){o(i.error)}})}function c(e,t,n){return t?new Promise(function(r,o){var i=1e3*t,c=[],s=e.transaction(h,"readwrite"),a=s.objectStore(h),u=a.index(l);u.openCursor().onsuccess=function(e){var t=e.target.result;if(t&&n-i>t.value[l]){var r=t.value[p];c.push(r),a.delete(r),t.continue()}},s.oncomplete=function(){r(c)},s.onabort=o}):Promise.resolve([])}function s(e,t){return t?new Promise(function(n,r){var o=[],i=e.transaction(h,"readwrite"),c=i.objectStore(h),s=c.index(l),a=s.count();s.count().onsuccess=function(){var e=a.result;e>t&&(s.openCursor().onsuccess=function(n){var r=n.target.result;if(r){var i=r.value[p];o.push(i),c.delete(i),e-o.length>t&&r.continue()}})},i.oncomplete=function(){n(o)},i.onabort=r}):Promise.resolve([])}function a(e,t,n,r){return c(e,n,r).then(function(n){return s(e,t).then(function(e){return n.concat(e)})})}var u="sw-toolbox-",f=1,h="store",p="url",l="timestamp",d={};t.exports={getDb:o,setTimestampForUrl:i,expireEntries:a}},{}],3:[function(e,t,n){"use strict";function r(e){var t=a.match(e.request);t?e.respondWith(t(e.request)):a.default&&"GET"===e.request.method&&0===e.request.url.indexOf("http")&&e.respondWith(a.default(e.request))}function o(e){s.debug("activate event fired");var t=u.cache.name+"$$$inactive$$$";e.waitUntil(s.renameCache(t,u.cache.name))}function i(e){return e.reduce(function(e,t){return e.concat(t)},[])}function c(e){var t=u.cache.name+"$$$inactive$$$";s.debug("install event fired"),s.debug("creating cache ["+t+"]"),e.waitUntil(s.openCache({cache:{name:t}}).then(function(e){return Promise.all(u.preCacheItems).then(i).then(s.validatePrecacheInput).then(function(t){return s.debug("preCache list: "+(t.join(", ")||"(none)")),e.addAll(t)})}))}e("serviceworker-cache-polyfill");var s=e("./helpers"),a=e("./router"),u=e("./options");t.exports={fetchListener:r,activateListener:o,installListener:c}},{"./helpers":1,"./options":4,"./router":6,"serviceworker-cache-polyfill":16}],4:[function(e,t,n){"use strict";var r;r=self.registration?self.registration.scope:self.scope||new URL("./",self.location).href,t.exports={cache:{name:"$$$toolbox-cache$$$"+r+"$$$",maxAgeSeconds:null,maxEntries:null},debug:!1,networkTimeoutSeconds:null,preCacheItems:[],successResponses:/^0|([123]\d\d)|(40[14567])|410$/}},{}],5:[function(e,t,n){"use strict";var r=new URL("./",self.location),o=r.pathname,i=e("path-to-regexp"),c=function(e,t,n,r){t instanceof RegExp?this.fullUrlRegExp=t:(0!==t.indexOf("/")&&(t=o+t),this.keys=[],this.regexp=i(t,this.keys)),this.method=e,this.options=r,this.handler=n};c.prototype.makeHandler=function(e){var t;if(this.regexp){var n=this.regexp.exec(e);t={},this.keys.forEach(function(e,r){t[e.name]=n[r+1]})}return function(e){return this.handler(e,t,this.options)}.bind(this)},t.exports=c},{"path-to-regexp":15}],6:[function(e,t,n){"use strict";function r(e){return e.replace(/[-\/\\^$*+?.()|[\]{}]/g,"\\$&")}var o=e("./route"),i=e("./helpers"),c=function(e,t){for(var n=e.entries(),r=n.next(),o=[];!r.done;){var i=new RegExp(r.value[0]);i.test(t)&&o.push(r.value[1]),r=n.next()}return o},s=function(){this.routes=new Map,this.routes.set(RegExp,new Map),this.default=null};["get","post","put","delete","head","any"].forEach(function(e){s.prototype[e]=function(t,n,r){return this.add(e,t,n,r)}}),s.prototype.add=function(e,t,n,c){c=c||{};var s;t instanceof RegExp?s=RegExp:(s=c.origin||self.location.origin,s=s instanceof RegExp?s.source:r(s)),e=e.toLowerCase();var a=new o(e,t,n,c);this.routes.has(s)||this.routes.set(s,new Map);var u=this.routes.get(s);u.has(e)||u.set(e,new Map);var f=u.get(e),h=a.regexp||a.fullUrlRegExp;f.has(h.source)&&i.debug('"'+t+'" resolves to same regex as existing route.'),f.set(h.source,a)},s.prototype.matchMethod=function(e,t){var n=new URL(t),r=n.origin,o=n.pathname;return this._match(e,c(this.routes,r),o)||this._match(e,[this.routes.get(RegExp)],t)},s.prototype._match=function(e,t,n){if(0===t.length)return null;for(var r=0;r<t.length;r++){var o=t[r],i=o&&o.get(e.toLowerCase());if(i){var s=c(i,n);if(s.length>0)return s[0].makeHandler(n)}}return null},s.prototype.match=function(e){return this.matchMethod(e.method,e.url)||this.matchMethod("any",e.url)},t.exports=new s},{"./helpers":1,"./route":5}],7:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache first ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(t){var r=n.cache||o.cache,c=Date.now();return i.isResponseFresh(t,r.maxAgeSeconds,c)?t:i.fetchAndCache(e,n)})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],8:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache only ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(e){var t=n.cache||o.cache,r=Date.now();if(i.isResponseFresh(e,t.maxAgeSeconds,r))return e})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],9:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: fastest ["+e.url+"]",n),new Promise(function(r,c){var s=!1,a=[],u=function(e){a.push(e.toString()),s?c(new Error('Both cache and network failed: "'+a.join('", "')+'"')):s=!0},f=function(e){e instanceof Response?r(e):u("No result returned")};o.fetchAndCache(e.clone(),n).then(f,u),i(e,t,n).then(f,u)})}var o=e("../helpers"),i=e("./cacheOnly");t.exports=r},{"../helpers":1,"./cacheOnly":8}],10:[function(e,t,n){t.exports={networkOnly:e("./networkOnly"),networkFirst:e("./networkFirst"),cacheOnly:e("./cacheOnly"),cacheFirst:e("./cacheFirst"),fastest:e("./fastest")}},{"./cacheFirst":7,"./cacheOnly":8,"./fastest":9,"./networkFirst":11,"./networkOnly":12}],11:[function(e,t,n){"use strict";function r(e,t,n){n=n||{};var r=n.successResponses||o.successResponses,c=n.networkTimeoutSeconds||o.networkTimeoutSeconds;return i.debug("Strategy: network first ["+e.url+"]",n),i.openCache(n).then(function(t){var s,a,u=[];if(c){var f=new Promise(function(r){s=setTimeout(function(){t.match(e).then(function(e){var t=n.cache||o.cache,c=Date.now(),s=t.maxAgeSeconds;i.isResponseFresh(e,s,c)&&r(e)})},1e3*c)});u.push(f)}var h=i.fetchAndCache(e,n).then(function(e){if(s&&clearTimeout(s),r.test(e.status))return e;throw i.debug("Response was an HTTP error: "+e.statusText,n),a=e,new Error("Bad response")}).catch(function(r){return i.debug("Network or response error, fallback to cache ["+e.url+"]",n),t.match(e).then(function(e){if(e)return e;if(a)return a;throw r})});return u.push(h),Promise.race(u)})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],12:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: network only ["+e.url+"]",n),fetch(e)}var o=e("../helpers");t.exports=r},{"../helpers":1}],13:[function(e,t,n){"use strict";var r=e("./options"),o=e("./router"),i=e("./helpers"),c=e("./strategies"),s=e("./listeners");i.debug("Service Worker Toolbox is loading"),self.addEventListener("install",s.installListener),self.addEventListener("activate",s.activateListener),self.addEventListener("fetch",s.fetchListener),t.exports={networkOnly:c.networkOnly,networkFirst:c.networkFirst,cacheOnly:c.cacheOnly,cacheFirst:c.cacheFirst,fastest:c.fastest,router:o,options:r,cache:i.cache,uncache:i.uncache,precache:i.precache}},{"./helpers":1,"./listeners":3,"./options":4,"./router":6,"./strategies":10}],14:[function(e,t,n){t.exports=Array.isArray||function(e){return"[object Array]"==Object.prototype.toString.call(e)}},{}],15:[function(e,t,n){function r(e,t){for(var n,r=[],o=0,i=0,c="",s=t&&t.delimiter||"/";null!=(n=x.exec(e));){var f=n[0],h=n[1],p=n.index;if(c+=e.slice(i,p),i=p+f.length,h)c+=h[1];else{var l=e[i],d=n[2],m=n[3],g=n[4],v=n[5],w=n[6],y=n[7];c&&(r.push(c),c="");var b=null!=d&&null!=l&&l!==d,E="+"===w||"*"===w,R="?"===w||"*"===w,k=n[2]||s,$=g||v;r.push({name:m||o++,prefix:d||"",delimiter:k,optional:R,repeat:E,partial:b,asterisk:!!y,pattern:$?u($):y?".*":"[^"+a(k)+"]+?"})}}return i<e.length&&(c+=e.substr(i)),c&&r.push(c),r}function o(e,t){return s(r(e,t))}function i(e){return encodeURI(e).replace(/[\/?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function c(e){return encodeURI(e).replace(/[?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function s(e){for(var t=new Array(e.length),n=0;n<e.length;n++)"object"==typeof e[n]&&(t[n]=new RegExp("^(?:"+e[n].pattern+")$"));return function(n,r){for(var o="",s=n||{},a=r||{},u=a.pretty?i:encodeURIComponent,f=0;f<e.length;f++){var h=e[f];if("string"!=typeof h){var p,l=s[h.name];if(null==l){if(h.optional){h.partial&&(o+=h.prefix);continue}throw new TypeError('Expected "'+h.name+'" to be defined')}if(v(l)){if(!h.repeat)throw new TypeError('Expected "'+h.name+'" to not repeat, but received `'+JSON.stringify(l)+"`");if(0===l.length){if(h.optional)continue;throw new TypeError('Expected "'+h.name+'" to not be empty')}for(var d=0;d<l.length;d++){if(p=u(l[d]),!t[f].test(p))throw new TypeError('Expected all "'+h.name+'" to match "'+h.pattern+'", but received `'+JSON.stringify(p)+"`");o+=(0===d?h.prefix:h.delimiter)+p}}else{if(p=h.asterisk?c(l):u(l),!t[f].test(p))throw new TypeError('Expected "'+h.name+'" to match "'+h.pattern+'", but received "'+p+'"');o+=h.prefix+p}}else o+=h}return o}}function a(e){return e.replace(/([.+*?=^!:${}()[\]|\/\\])/g,"\\$1")}function u(e){return e.replace(/([=!:$\/()])/g,"\\$1")}function f(e,t){return e.keys=t,e}function h(e){return e.sensitive?"":"i"}function p(e,t){var n=e.source.match(/\((?!\?)/g);if(n)for(var r=0;r<n.length;r++)t.push({name:r,prefix:null,delimiter:null,optional:!1,repeat:!1,partial:!1,asterisk:!1,pattern:null});return f(e,t)}function l(e,t,n){for(var r=[],o=0;o<e.length;o++)r.push(g(e[o],t,n).source);var i=new RegExp("(?:"+r.join("|")+")",h(n));return f(i,t)}function d(e,t,n){return m(r(e,n),t,n)}function m(e,t,n){v(t)||(n=t||n,t=[]),n=n||{};for(var r=n.strict,o=n.end!==!1,i="",c=0;c<e.length;c++){var s=e[c];if("string"==typeof s)i+=a(s);else{var u=a(s.prefix),p="(?:"+s.pattern+")";t.push(s),s.repeat&&(p+="(?:"+u+p+")*"),p=s.optional?s.partial?u+"("+p+")?":"(?:"+u+"("+p+"))?":u+"("+p+")",i+=p}}var l=a(n.delimiter||"/"),d=i.slice(-l.length)===l;return r||(i=(d?i.slice(0,-l.length):i)+"(?:"+l+"(?=$))?"),i+=o?"$":r&&d?"":"(?="+l+"|$)",f(new RegExp("^"+i,h(n)),t)}function g(e,t,n){return v(t)||(n=t||n,t=[]),n=n||{},e instanceof RegExp?p(e,t):v(e)?l(e,t,n):d(e,t,n)}var v=e("isarray");t.exports=g,t.exports.parse=r,t.exports.compile=o,t.exports.tokensToFunction=s,t.exports.tokensToRegExp=m;var x=new RegExp(["(\\\\.)","([\\/.])?(?:(?:\\:(\\w+)(?:\\(((?:\\\\.|[^\\\\()])+)\\))?|\\(((?:\\\\.|[^\\\\()])+)\\))([+*?])?|(\\*))"].join("|"),"g")},{isarray:14}],16:[function(e,t,n){!function(){var e=Cache.prototype.addAll,t=navigator.userAgent.match(/(Firefox|Chrome)\/(\d+\.)/);if(t)var n=t[1],r=parseInt(t[2]);e&&(!t||"Firefox"===n&&r>=46||"Chrome"===n&&r>=50)||(Cache.prototype.addAll=function(e){function t(e){this.name="NetworkError",this.code=19,this.message=e}var n=this;return t.prototype=Object.create(Error.prototype),Promise.resolve().then(function(){if(arguments.length<1)throw new TypeError;return e=e.map(function(e){return e instanceof Request?e:String(e)}),Promise.all(e.map(function(e){"string"==typeof e&&(e=new Request(e));var n=new URL(e.url).protocol;if("http:"!==n&&"https:"!==n)throw new t("Invalid scheme");return fetch(e.clone())}))}).then(function(r){if(r.some(function(e){return!e.ok}))throw new t("Incorrect response status");return Promise.all(r.map(function(t,r){return n.put(e[r],t)}))}).then(function(){})},Cache.prototype.add=function(e){return this.addAll([e])})}()},{}]},{},[13])(13)});


// *** End of auto-included sw-toolbox code. ***



// Runtime cache 配置转换后的 toolbox 代码.

toolbox.router.get("/*", toolbox.cacheFirst, {"origin":"unpkg.com"});
toolbox.router.get("/npm/*", toolbox.cacheFirst, {"origin":"cdn.jsdelivr.net"});





/* eslint-enable */
