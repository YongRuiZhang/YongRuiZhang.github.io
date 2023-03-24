/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","855f949b9eb2e9068cf9936bbe20c41b"],["/about/index.html","544a1a8411ebff1287fe24d43f630dc5"],["/archives/2023/01/index.html","fe5000def3da43e09a31cc3e4549d159"],["/archives/2023/02/index.html","bd2113ce99e3593b9daf313b8757363c"],["/archives/2023/02/page/2/index.html","cd8ff14c27d4a2401d79b4eb865d4753"],["/archives/2023/03/index.html","e2f0ec8898541e0cfffd35b8f806c2a5"],["/archives/2023/index.html","f327daeb91c948e2ddea35b1b540b722"],["/archives/2023/page/2/index.html","cca7eb5470e696d4a7459712d8ec6c97"],["/archives/2023/page/3/index.html","3112160bacfc7957e80c1b0649aecac5"],["/archives/2023/page/4/index.html","b1e450cb3633df0443b409d4e9bcdc85"],["/archives/index.html","1b669e41a84c5c285c2ffd76ef9c1e0f"],["/archives/page/2/index.html","6115345508ffc1c0ac9ddf31c191a313"],["/archives/page/3/index.html","7338dbc4109bf2e8e5a09e58941ce735"],["/archives/page/4/index.html","0a3f40bc6549099f7b5f6dbb461184ec"],["/categories/Java/index.html","93351ae72d65e235fd93edb3b36b8a3c"],["/categories/Java/后端/index.html","bbad93540f29ff4d7f70dd6be0da8ae4"],["/categories/Java/基础/index.html","cc0a81ff9ae107168e893b400dda0410"],["/categories/Java/基础/集合/index.html","fa09d12235f98fb269127223234c4f1d"],["/categories/Python/index.html","bd3cec3384420d1b750db4570769562e"],["/categories/Python/编程环境/index.html","e70fb2cea6f53413147cb71f05a5fd17"],["/categories/R语言/index.html","8df2d5c9b18f67fa7982ced7b9b18b31"],["/categories/R语言/编程环境/index.html","ab974770591e2ae1ea9f22a53ef43a5f"],["/categories/index.html","5531b1617a312b1b5b2fcc6d598fe09a"],["/categories/大数据开发/ElasticSearch/index.html","f7550c31db37a4d6e4ffaa60f9010f06"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","90e37fd78de3b3d110440c742f4c486b"],["/categories/大数据开发/HBase/index.html","5ad9bccc8622c7ff6d989b3a29663d1e"],["/categories/大数据开发/HBase/环境搭建/index.html","18b8b132eac8021f390382a68f4f5e46"],["/categories/大数据开发/Hadoop/index.html","b0336f9ff658c1cca9361710768cb731"],["/categories/大数据开发/Hadoop/环境搭建/index.html","4e00f6bd88009dfd71d3fb6c30e3e8ef"],["/categories/大数据开发/Zookeeper/index.html","9ba6c72c02f0c4a46945b73d515280ee"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","063b10433f962f99eba3e41446f63a77"],["/categories/大数据开发/index.html","9bcdc1ec8f6fc6cfaf858f3862b16ba5"],["/categories/操作系统/Linux/index.html","736b7f0bdb9f02ecad4608c2aee0f1e9"],["/categories/操作系统/Mac/index.html","9a4a433a0e5743189fbf88c4aa9688a9"],["/categories/操作系统/Windows/index.html","eb41fdc884ead0e38d8bf9b2cf592c82"],["/categories/操作系统/index.html","cd2a767c9a348c728d8dd18357550e84"],["/categories/数学建模/index.html","4a8ab32f1d06b78f30dc05dbf1bc5050"],["/categories/数学建模/latex/index.html","95891195670585e77f7979535c22a4b5"],["/categories/数学建模/优化类/index.html","8185453d86e1b84ad8f6196e2fd0b47f"],["/categories/数学建模/优化类/现代优化算法/index.html","9ef997a9a9439e93a69900e249f940bc"],["/categories/数学建模/优化类/规划类/index.html","5c402299ae98b29fba86e6ffd1d3f0ac"],["/categories/数学建模/绘图/index.html","07a70124855b66b2474f78c05b9e2979"],["/categories/数据库/MySQL/index.html","4f8c30538d0209144a4ae04731d8dbeb"],["/categories/数据库/index.html","b1232696d5ee543982876275dd88b2ef"],["/categories/数据结构和算法/index.html","15b08285d2ebb885cbeacd27b9aac551"],["/categories/数据结构和算法/page/2/index.html","5b414af0a469720652af4a2d7446dffc"],["/categories/数据结构和算法/基本原理/bfs/index.html","1a0d720052136a076e82e602641d0552"],["/categories/数据结构和算法/基本原理/dfs/index.html","8ba48b9bc5fa9ab609e568a294a399d6"],["/categories/数据结构和算法/基本原理/index.html","a4e4c22bdb98292aad448ff36fb860a1"],["/categories/数据结构和算法/基本原理/动态规划/index.html","3237dd3455bdaea6f525e42db30703a6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","889cc86170afd64085cd7dc1404bcd8f"],["/categories/数据结构和算法/基本原理/图论/index.html","f581297429e3a0a25c79130d2e1ac20d"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","8b8787f48f2d55d7818504087a9dae79"],["/categories/数据结构和算法/基本原理/数论/index.html","0a8a9b576522600759467660d3ed86e7"],["/categories/数据结构和算法/基本原理/树论/index.html","920ee5e4adac9f7639b4afeddb2bd23f"],["/categories/数据结构和算法/基本原理/链表/index.html","72bb96874adca7476299fb8d133f819c"],["/categories/数据结构和算法/算法题/index.html","277669abc0f42c2d190a619b4abe56c3"],["/categories/数据结构和算法/算法题/二分查找/index.html","125ac7e5a068c693feab0a24c2aef30b"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0be94810201cd639e48ebaba2a1199c4"],["/categories/数据结构和算法/算法题/动态规划/index.html","4e3363aa32c05b9130b794c93250b86e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","9e7e2f1f73fb388cadca8690e5029ce4"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","4ab4d0937a348fb3fdc9461441699831"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","e1d322dcea48a891b4bffde00718553a"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","6663494bce86e53410ba9488f0acc0be"],["/categories/数据结构和算法/算法题/栈和队列/index.html","0091da522a01d49cd947a18bcee671c9"],["/categories/数据结构和算法/算法题/树论/index.html","b6a5bb868fda85c65e98658fb69279db"],["/categories/杂七杂八/index.html","ff2495727c12a154cd3b943280453c4a"],["/categories/杂七杂八/博客搭建/index.html","275fcc2f5b1feddf56e511c5781004c3"],["/categories/编程环境/index.html","584a23565502062334e3790617398b58"],["/categories/英语学习/index.html","12c94829bb32ccbeee5b632d0df97003"],["/categories/英语学习/英语语法/index.html","28e229a167f5e02b1f91d6bb72157f11"],["/comments/index.html","2f909ceddc48a805a02b6f67dcad8da2"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","8325c8719a21311bddf3162fce3c4489"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","2a522f952c398f102ed37aa5832ab349"],["/movies/index.html","e0a926fed84d891642b85b41d2a920ad"],["/music/index.html","6411a73ae32df27a09976d4c03812afc"],["/page/2/index.html","b2b62892e2079f34b7986f174f03a177"],["/page/3/index.html","a5478d1c6342818315f48b27b2176a6c"],["/page/4/index.html","23a63f6810906b410763158272ea0648"],["/page/5/index.html","ee2344fa40bb8eb10f07a3de0250a35a"],["/posts/1021360842.html","2a2341d5486fdfeee085d162c14a62d8"],["/posts/1120620192.html","e482741ed7546495087179bd593aa0fd"],["/posts/1141628095.html","a5951594ea7dfcaff13c421b0cb222d7"],["/posts/1168613674.html","af7eaf7dd28b3444d65f86dcf5913537"],["/posts/1219920510.html","014cbc9f43ea0e275155eaadace02ab4"],["/posts/1222166338.html","0135e7710c83f8faf5a6ead908c91947"],["/posts/1259097482.html","a54d8f654c4c3cc05c9b3753cb02f54c"],["/posts/1271036369.html","6e80c5dda7431cf432825477d79fafc5"],["/posts/1312847445.html","5488c0b94da25b4ebfa2734e31c39249"],["/posts/135355774.html","f4fc75e426b76259081ebbbeda6630d2"],["/posts/1375344716.html","b2908e84da2799dad50de3009c0ed0b5"],["/posts/1388991698.html","39bb52393a580fb9f8d01bcab53c5f5b"],["/posts/1410315814.html","13985023219550185250a966f246ba15"],["/posts/1452790229.html","c59711b59449ed9adf8b152715d79174"],["/posts/1470079884.html","bad0f2aed9323ea5d924078630b03545"],["/posts/1470079885.html","7bf4edbf6de35bc1061bd639e8da4453"],["/posts/1470079886.html","11c3b0367ef1c2ffff5d742a9917d104"],["/posts/1470079887.html","ea11478ea09a62aed87f6237ef0c733c"],["/posts/1498536549.html","e844785f7ca23f7b0b1f84c5502af7e7"],["/posts/1557866301.html","4f34fb2341751e905397ff06959f2c3b"],["/posts/1571776361.html","bec1a7874d77438f7cfcd8b51376ebcb"],["/posts/1605124548.html","7dac53809a884d44b3d2d7b7a3eb6d1f"],["/posts/1633036852.html","d0063c8ebed5cc9289dc980a71a8d206"],["/posts/1765123828.html","df2a02769c0b7ee458f285fc9dcc8050"],["/posts/1776114197.html","7c41e7654273e0897e5a704d8a94ac54"],["/posts/1817748743.html","84b50a1d17a88d89425746e77e7999a2"],["/posts/1925125395.html","d532065b17c58213fb71c4e8c8b9f1d9"],["/posts/1966191251.html","4f555d701d94ce113df12b12c18d8e0c"],["/posts/1987617322.html","d723c3bc58eda7f8a4db84f7f163b2c8"],["/posts/1999788039.html","5d74105ef1a0fd85d9fb727a679bb457"],["/posts/2075104059.html","610c7e19a3166853345e9879bde164c6"],["/posts/2087796737.html","d58c633389a83626dbdaede4c8cd9a20"],["/posts/2207806286.html","4523d8ac2948cb92be3c8e7ff6a0c600"],["/posts/2225903441.html","e2accbbd23149ced770a8cfc6ca28372"],["/posts/2265610284.html","59cb06a9830c9d280612dafe215590e6"],["/posts/2281352001.html","7c6fb7e1ebe59143b6336caca3060b68"],["/posts/2364755265.html","801443dc8459fb04fa022f981b56e4f3"],["/posts/2414116852.html","e17de0f0f22a54f1a7e6d00667134c88"],["/posts/2482902029.html","894061ae5befdaaab2427ba140aabf3a"],["/posts/2495386210.html","38f757aa66cc2d9f0170b1f0c85d707a"],["/posts/2516528882.html","d0e68e781101bac16bd3ebdede70463e"],["/posts/2526659543.html","07baf375b7f09d4d34eb7082e57641f3"],["/posts/2529807823.html","eaca5a8de2f92da4825d64f3a6cb6e43"],["/posts/2742438348.html","b13353d03904b181058dc3893553bfec"],["/posts/2888309600.html","17b3b36a4bb6aacc2f341768528989d0"],["/posts/2891591958.html","82b20b618b8a4f7a30092eb30c72f3e5"],["/posts/2909934084.html","a495f9797005d71260ac1caa0acbda8a"],["/posts/2920256992.html","d10d6115f4b8795b3d15b7d1c79edb98"],["/posts/3005926051.html","70abe5ed37767d120b29e968a461e547"],["/posts/3169224211.html","fc771cc53cc86ad2aacfef00355991d0"],["/posts/3259212833.html","f64317f5860ba78a34947ae0d1b1e767"],["/posts/3266130344.html","e30988b6543c43abfa879ddf855332a5"],["/posts/3306641566.html","125578b95cf7d55396a01946c2f72541"],["/posts/3312011324.html","b3d209d7f7e4382dc379f3b5066088e8"],["/posts/336911618.html","2120e5a70ec81b5a378d157d891a0f3c"],["/posts/3402121571.html","381202ed1bf22ed26f5fd4468f3db1e6"],["/posts/3405577485.html","adf9105ca17e98053fe510c19cf455e9"],["/posts/3498516849.html","e0a0ba3723222273354d7a36f50adab8"],["/posts/3513711414.html","3fdcf742950d63f9408a4e6c985eb03d"],["/posts/3546711884.html","c7d6408e0440eaf6c98a1b0ef8cd3611"],["/posts/3731385230.html","4f22fa7b3dde6324bc84ab17fcb44857"],["/posts/3772089482.html","5570f0a9d07123ef7528a46a955fbf95"],["/posts/386609427.html","f511d7e27db0fcc1366cc6976390f71b"],["/posts/4044235327.html","f7944f1616180a1f39482b760b33c0e8"],["/posts/4115971639.html","d6635d8ca72a6e978305e401092392f1"],["/posts/4130790367.html","b117a4361df87597545fece1a4c8a595"],["/posts/4131986683.html","2df9acd88524a9306d21d84b6c30fb71"],["/posts/4177218757.html","4ecee42229d0418059c72a8cea29b569"],["/posts/4192183953.html","9d71681868d13450dfd6d125fa71313c"],["/posts/4261103898.html","54a0ef000fc6cd8a9c92410d77138ced"],["/posts/482495853.html","0203b5476554ad67bf73dc983b794d3f"],["/posts/488247922.html","03f5e91001dab9af47158fd6f7867d5a"],["/posts/570165348.html","cbe35e3b1ef17a9672128705968e98be"],["/posts/595890772.html","0d27fcdb3335164a24a46c7aa1327250"],["/posts/694347442.html","732a64ae7f5b2cf130a9515c528d267e"],["/posts/707384687.html","ba8b176922ed5fbd907e7fb121dea1a8"],["/posts/71180092.html","7b5c7cb6a4cc03e7d0487a3e4373a876"],["/posts/716459272.html","3d102b0775c619b7d0a1e0b3f6491e38"],["/posts/795397410.html","1e87df3b13bb10fc8bf8b736a093fe40"],["/posts/820223701.html","f8c1dc7add26d5329c348800df115564"],["/posts/830372185.html","900ea3ee9d7587a75f956a11d06aa29f"],["/posts/88294277.html","ee5910ba1bb7802a866c5a5932d0822c"],["/posts/939963535.html","9cd997b2100c5722caba8a767e2e0ad6"],["/posts/983786067.html","4053e9d3cbd6a72538d8d567bd72c38d"],["/sw-register.js","ce66d83d0ad15268b3426de891f08163"],["/tags/C/index.html","d90a9b73ab2aedbb8d71ef1290371f3d"],["/tags/C/page/2/index.html","ce0f0096a52d9742c3965106c2e97323"],["/tags/C/page/3/index.html","3480c579d75bd41b0d493bb3fe0ad3ef"],["/tags/ElasticSearch/index.html","5597f02a0386a58c2fb0c8e3aeecc29a"],["/tags/GUI/index.html","e96729a24c60b64f33d6fce33b18e0b9"],["/tags/HBase/index.html","1286c8787b1f19d5d9b5c3b245e738dd"],["/tags/Hadoop/index.html","59c7f73bdadf1b457dbc02b1c29005a4"],["/tags/Java/index.html","f2c74bcb402919e315dac72a5fbd717e"],["/tags/Java后端/index.html","efdd4153a16b1c391ca7d3cb44aae1f8"],["/tags/Java基础/index.html","1822c214c63aa7776ce229511f5df03a"],["/tags/Java基础/page/2/index.html","45a98c3912dfea92eac618d3f2395aef"],["/tags/Kibana/index.html","5597b3703a19e4c1d9adc2bae74a02fb"],["/tags/Linux/index.html","3514fe11d414df9dc57f22271c9e4cce"],["/tags/Linux/page/2/index.html","e44b7ed88fb319c4821f1214a3b96ebd"],["/tags/Mac/index.html","bb12b9987b538842b7e0dbf9ff30b784"],["/tags/Mac/page/2/index.html","dc0a6d5fd79e733b2dab94db5952f329"],["/tags/Maven/index.html","600bdabc615d03792693bcb9c7e7a00f"],["/tags/MySQL/index.html","e0d8a51d2852883b9476f816d3ecf377"],["/tags/Python/index.html","eedf3cdb9d2e287526be320ce5b7c75a"],["/tags/R语言/index.html","cdcae46ba0ac096eb7ae6e30b410c4bb"],["/tags/Ubuntu/index.html","97c05d6400e96410ba7b978c36cde74c"],["/tags/Windows/index.html","d74d337dbe1cae2ecfa0e7b42ef8d7d3"],["/tags/ZooKeeper/index.html","3dddd3d5c9803c77791fdfd15eb3644b"],["/tags/bfs/index.html","d112910c07b830caaa1995f89fa5dafc"],["/tags/dfs/index.html","1cb890b355b6790f8e7e5b5b4ebb6ce2"],["/tags/folium/index.html","bbe3159099a73f2c052cfe6ecdc2f103"],["/tags/git/index.html","4f68e8613b349c0ccd727e7981b4b262"],["/tags/index.html","b53cadd3b923967e92a16745a7c7c417"],["/tags/latex/index.html","ab39c0e52ba9c17dd84761d97f17e0de"],["/tags/二分查找/index.html","3a4c13b935c108bbca53dd7fb585f3c4"],["/tags/优化类/index.html","d3fac1d73e433fef7a4b67939fea6bd8"],["/tags/前缀和与差分/index.html","b3e0271037d93a0468cfa740d610f8e9"],["/tags/动态规划/index.html","f8760cb832064e621a421816a0da98a8"],["/tags/动态规划/page/2/index.html","5d48fd9d57e7ff94c4fcb52cc84783b8"],["/tags/博客搭建/index.html","8dbc17bc14e8d34de03f5f2705620c3b"],["/tags/图论/index.html","0cb0703a7f1168794dcfd34b64485ac8"],["/tags/大数据/index.html","2b5036629a95cfd039367b5c0bf190b0"],["/tags/大数据/page/2/index.html","cc68f02e024e56122100e97b9b71244b"],["/tags/操作系统/index.html","89d4f5e7777f938a96104a3545344fb9"],["/tags/数学建模/index.html","63fc8f80d0c669fa75189aaab3aa2712"],["/tags/数据库/index.html","16a59414a3453cceea841dc9f9b46bf3"],["/tags/数据结构和算法/index.html","f7b228df049d344412d6d90d79169870"],["/tags/数据结构和算法/page/2/index.html","f680f974527fdd7d1ded82b1a5333673"],["/tags/数据结构和算法/page/3/index.html","352d6868ec7361eefdeceb0e42a32bf1"],["/tags/数组和字符串/index.html","3be18b61f3219b4ca16a82f44a9036cb"],["/tags/枚举类/index.html","2f2e0f83f0673f28ecb2ded44a329515"],["/tags/栈和队列/index.html","dbd0511e01ef81f27280e57c098fbdee"],["/tags/树论/index.html","6bbacfb0c189cfd42a2568d816f0a145"],["/tags/测试/index.html","e94af8bc5f9169ce920d727493ce1c7b"],["/tags/环境/index.html","8e1740715993f3533346987673585c44"],["/tags/环境变量/index.html","5535df07e2eceb1b54e3066801b11cf1"],["/tags/绘图/index.html","1bd14a695758f68af92f7ecf9c0ce177"],["/tags/编程环境/index.html","8aec3b62cc7e057fe7495fc828b2f9c0"],["/tags/网络编程/index.html","e0dc9522438f0e8711491c7a6d95ca78"],["/tags/英语语法/index.html","03f31da434f7be65faacfaf7900d6d0e"],["/tags/论文/index.html","68c212fa1e34414e81fb5636336dfb55"],["/tags/资源下载/index.html","293b7604f2a37333625701ec09d572e5"],["/tags/链表/index.html","13d7880802931744b9404fe5e7f6c439"],["/tags/集合/index.html","626f8b7c81a08d98315de3e109cf56c7"],["/tags/集群/index.html","29c2987709b873d37526ecedbd648ef1"]];
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
