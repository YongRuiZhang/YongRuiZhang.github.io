/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","f6164dafbbe19d731aaa4daec62932a7"],["/about/index.html","1d80566e71e97e50b931f8fe850c4163"],["/archives/2023/01/index.html","28afa526cc8adc8e57aa43d7728f689b"],["/archives/2023/02/index.html","d5888eabee6ea4b36859387114dbb83e"],["/archives/2023/02/page/2/index.html","234eabffb4012e079ba32e293c70a9ec"],["/archives/2023/03/index.html","7d3668478bf48b5cca3342bb02203898"],["/archives/2023/05/index.html","d6eede3831b154cacb66e3ec42d1340a"],["/archives/2023/06/index.html","4fca3b9a420789b90a5be1c20074f463"],["/archives/2023/09/index.html","01c32c1be536fdf9d5dceadbeb5f23fd"],["/archives/2023/11/index.html","1af2229480ec9ddd1b17e87263374cdc"],["/archives/2023/12/index.html","1677a1ca451415e50d1cd93dbebc3aee"],["/archives/2023/index.html","057d1560aeeb01f474cbafc2ee64d970"],["/archives/2023/page/2/index.html","7576b20888af96eb8b39c1077eac172c"],["/archives/2023/page/3/index.html","4c3c968d61ac9c556af9f0b7c7d97bc2"],["/archives/2023/page/4/index.html","76c71c89a38188ceffe1761e3dd6321c"],["/archives/2024/02/index.html","fc757cfe405fdab9d7695580e5857099"],["/archives/2024/index.html","a6501d47dbba216c0cd67b7ed6938a3b"],["/archives/index.html","9b89d05604ac090508f1fcdf989a8b29"],["/archives/page/2/index.html","f4adf4592c50d3d6ffdffb0a905850a7"],["/archives/page/3/index.html","19c3228fe76c19be9ee7832ae90cd57f"],["/archives/page/4/index.html","6df397561d975694cdce4a21808cf191"],["/baidu_verify_codeva-qQP2iZOMLX.html","f776c45caa3e30170294c34018cc5f52"],["/categories/Java/index.html","1251c188b61398f8ca5ded0b6033022f"],["/categories/Java/后端/index.html","e4f69c3e1901882850c605414fdb5564"],["/categories/Java/基础/index.html","89e62ce31022edd05f20181168fd0449"],["/categories/Java/基础/集合/index.html","2d4964e3da3e63e49b451999721080b3"],["/categories/Python/index.html","f12e859fea095b5336e7619951907467"],["/categories/Python/编程环境/index.html","d2e5a2d067cf85f754066fc624a80df5"],["/categories/R语言/index.html","5f633acfab51cb33c8b7925ce17cff71"],["/categories/R语言/编程环境/index.html","3b1797e0dd8216dd4ced522baa07ddc0"],["/categories/index.html","683b404d9cf9cacdcd1f3e0d4da22f33"],["/categories/中间件/index.html","5aa7439c31050816288db2c18f4cf48b"],["/categories/前端/Vue/index.html","6e466c9012fc741e0a063b9983970fdb"],["/categories/前端/index.html","3c8c2a6949a6b3c5775f184f6c2b16e1"],["/categories/大数据开发/ElasticSearch/index.html","4db62c74b04e5921202b02a96ef49cc0"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","dce3d5439286884164eaa6eff47c7bd0"],["/categories/大数据开发/HBase/index.html","9d837a36c6cb495b2f400512f85888ec"],["/categories/大数据开发/HBase/学习笔记/index.html","7a0d9565c354e56119c710e260a92fb3"],["/categories/大数据开发/HBase/环境搭建/index.html","443598a0094a105958110eec5cc2f658"],["/categories/大数据开发/Hadoop/index.html","5bd87417533922f689b92d6768e2c63e"],["/categories/大数据开发/Hadoop/技术/index.html","81414de2efe892b0a1abb542de1cc38e"],["/categories/大数据开发/Hadoop/环境搭建/index.html","540917234ce223a4f1d6e1efc5e640fa"],["/categories/大数据开发/Redis/index.html","4058f52bd384a63f03e3cddc50e4d82d"],["/categories/大数据开发/Redis/技术/index.html","bc8a5569fd231fd3628c2fba0a5bdb89"],["/categories/大数据开发/Redis/环境搭建/index.html","ac0d67237eafe614a4ac5ed127b8756a"],["/categories/大数据开发/Spark/index.html","129d6925980c291729785f4244b5d63c"],["/categories/大数据开发/Spark/环境搭建/index.html","0120476e6a2037e62e408ecc316bce69"],["/categories/大数据开发/Zookeeper/index.html","f5100e6a827a67a45de50fa2d7afc168"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","23bab1ecf59b2fe3071c0ee6bc5af6bc"],["/categories/大数据开发/index.html","777b01ba5449360875733c6ce16e21b4"],["/categories/学校课程/index.html","c64b1e1fd97bab71929006f3987300b3"],["/categories/学校课程/计算机操作系统/index.html","5157e0e8b9f140d99fd83cee83e989d4"],["/categories/操作系统/Linux/index.html","9d7b8bbeba27c3c040d80e8d219569b1"],["/categories/操作系统/Mac/index.html","9c5a849c74bc70541aab67b1685c7ca2"],["/categories/操作系统/Windows/index.html","f92f6bc46245e5fa0359a9d0f5271608"],["/categories/操作系统/index.html","a238f395b23057b92e2379804abba8cb"],["/categories/数学建模/index.html","9ec74a2df76f1e0cbbf536c33e791d9f"],["/categories/数学建模/latex/index.html","9ea4f7409027ef3262237380cb63ae67"],["/categories/数学建模/优化类/index.html","be8756c10ff8376ae6ec73b75c1e10a1"],["/categories/数学建模/优化类/现代优化算法/index.html","ec8be67e54113638f010752159176713"],["/categories/数学建模/优化类/规划类/index.html","b777419c08ffe5445c72b59beb0af6eb"],["/categories/数学建模/绘图/index.html","40a4adffa035b7a59b9e0afac2a7a240"],["/categories/数据库/MySQL/index.html","e44ee62acc6ac05f8bf81fc11648f31a"],["/categories/数据库/index.html","0521cb103d9bb8b23e15f2120196e8df"],["/categories/数据结构和算法/index.html","8c8c1082bb513b70ec91aaeed5c2b5f5"],["/categories/数据结构和算法/page/2/index.html","555062096eee98ed4400f20b4aba23e6"],["/categories/数据结构和算法/基本原理/bfs/index.html","e5b9913b4194ee4323663abdf24002aa"],["/categories/数据结构和算法/基本原理/dfs/index.html","19129c6d74c333b47b46459fec47f58c"],["/categories/数据结构和算法/基本原理/index.html","8950a0113756c5c7a53e5877e547349e"],["/categories/数据结构和算法/基本原理/动态规划/index.html","8d205e14bd7b0513ad99a9bffb608c3f"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","d8a186a3133b51acfcf7ba0418881d6b"],["/categories/数据结构和算法/基本原理/图论/index.html","f61270e7d3df93409f574f2f4a965b7c"],["/categories/数据结构和算法/基本原理/字符串/index.html","72d25106430032fcc51034fe3ab9c85e"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","137fdd6bb1d9f2365e791708799be164"],["/categories/数据结构和算法/基本原理/数论/index.html","5a0faf3bde0e9ff6399644eff7e6398c"],["/categories/数据结构和算法/基本原理/树论/index.html","603aba0ed3d26b2a1b3fa09158b31f1a"],["/categories/数据结构和算法/基本原理/链表/index.html","92198da33847121b54d89e3d45c6ad27"],["/categories/数据结构和算法/算法题/index.html","1d3d73910bb6c231c882473d76867057"],["/categories/数据结构和算法/算法题/二分查找/index.html","815aabc28c187366dfc63b6a83cf4a79"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f16e9703633731f45bd7f30169cc9aba"],["/categories/数据结构和算法/算法题/动态规划/index.html","23edd6f5098543094950406bacf83aa6"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","171172f8b47a98da2d80e472b86d57dc"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","bc33f5904d404df624ca0f8522b9182a"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","51fce864f9a1d6863553cb9e96522163"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","ef83e2d6ab82e7c128398ffd48f75c91"],["/categories/数据结构和算法/算法题/数论/index.html","1cbe3feceac4f6fdf32e552b02f39db7"],["/categories/数据结构和算法/算法题/栈和队列/index.html","4548068efe66362e84628be918a314ef"],["/categories/数据结构和算法/算法题/树论/index.html","e903c25c8b97f996028b7b4917cf0909"],["/categories/杂七杂八/index.html","0be24921bb8f7c847b955b70f2d71a6c"],["/categories/杂七杂八/博客搭建/index.html","658f1b8e06c4d9817972e2ae5f0c200f"],["/categories/编程工具下载/index.html","1e9b43e0ae870f908e9b04811fdc0d99"],["/categories/编程环境/index.html","8df0700d2e317547f3bdb805ae50db8f"],["/categories/编程环境/大数据/index.html","d5a7cce2e5968120498c673c45f0f830"],["/categories/英语学习/index.html","a9b8b81cede6ef8c2fbd077805bbad84"],["/categories/英语学习/英语语法/index.html","764c363cce939810987727c5daea83fe"],["/comments/index.html","00fb128601bc91bfa69ca143dfe37780"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","0b73adf2fb99de16d3fb1c17a4e3b71a"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","9ba5a6298993b39d837490ef36df1bef"],["/movies/index.html","9dfd2ea078b3cf1237234d431c4bb0a1"],["/music/index.html","ee988d6d4485bc37331e825e9e1bc641"],["/page/2/index.html","a1affa27f9929bc58e35346a181f40fe"],["/page/3/index.html","28cbaba65e4aff5519e9f60e3f63e1a1"],["/page/4/index.html","2a67d4578b0c3df2724b422c0cffabe0"],["/page/5/index.html","1bac0626035bab3ead96d5ad671aeb17"],["/page/6/index.html","eb66eaa4efbe26751080314f8e296706"],["/posts/1021360842.html","e65f6669238fcf2dd447d0d7d5c7b051"],["/posts/1120620192.html","68aa38db8dd56421a5932365f60c26b8"],["/posts/1141628095.html","a7bdd746aef2988b32ba86826758b998"],["/posts/1168613674.html","6a788d929ee05c6a5a568fe44ec40e65"],["/posts/1219920510.html","e991748848f09a86cb80220d5ad81349"],["/posts/1222166338.html","dba3bec2571e5d85dafae852559bb995"],["/posts/1259097482.html","1417caa13bfbc70f385ff1889f5ed206"],["/posts/1271036369.html","e75fcd4c3d44ae8a8754778ec6ce91d5"],["/posts/1312847445.html","5b2f0476500973802b961aaca6dcf1ae"],["/posts/135355774.html","6fbc8b5c9ed1dc9c039fd8c62ac45050"],["/posts/1375344716.html","7cea5c9fa73dc14ad41099465f5c30b4"],["/posts/1388991698.html","fa40cd02623586adacc7d951f74abf2b"],["/posts/1410315814.html","74cce124cffc4f5d13bafad7be6b3db6"],["/posts/1452790229.html","fda7c02d4a9d5ae805e16fdafd7ea01b"],["/posts/1470079884.html","5f0eb52eaca83dbc9476cb19784bfdad"],["/posts/1470079885.html","5a29d8b6f9e54059c825e9caf7dfa622"],["/posts/1470079886.html","1ed67b24f5d539cc79141a937c3009ba"],["/posts/1470079887.html","98b8cefc2a31fb7d59a40100350346fa"],["/posts/1498536549.html","9bd114d07de60fb2096379c8e02d6af2"],["/posts/1539568593.html","9b34dbf7b51256a15ad0f2c196e8fa77"],["/posts/1547067935.html","e1647665bdb051f4621d8d196ee8ba33"],["/posts/1557866301.html","beff46609bb3636612773d9caf708237"],["/posts/1571776361.html","911ef856020537faab9775fac51facf2"],["/posts/1605124548.html","4089372f9dcc05871de551aca7568429"],["/posts/1633036852.html","4456bd661d932ef34c90b00c212b807e"],["/posts/1674202625.html","cedd6ea72c7fa6769986aebe3eadd716"],["/posts/1765123828.html","cbd14f30e2f375c4f2676eaff0fe67e4"],["/posts/1767336200.html","a9b6ac90035d600ba8ca68dd57be35d6"],["/posts/1776114197.html","eacf026ee89130da8bf2a2b78018a4a4"],["/posts/1817748743.html","680093d592edddca2fea8639f0b29a22"],["/posts/1925125395.html","36d4ea3cb3ccc0d015b35948347949ea"],["/posts/1966191251.html","1424f7c688942e619a917716d75e318e"],["/posts/1987617322.html","fa26a5d165360647aece1edcc4fc7f8f"],["/posts/1999788039.html","63517cbb867399dad01429962486fe01"],["/posts/2075104059.html","379a273d00c4506be67f9c188c751d69"],["/posts/2087796737.html","ff3467a85401e9b23cfb184d56c08816"],["/posts/2106547339.html","8c14b640804bd9cd79f7630148e4d2fa"],["/posts/2207806286.html","83b3f1784cec2370228ffdfc38ca4f7f"],["/posts/2225903441.html","d01be65faf1dca69762b644466f50e71"],["/posts/2265610284.html","b62ee71707c58f715e338c83b041651c"],["/posts/2281352001.html","6d0fba218779a549b58d2d6a2c34d558"],["/posts/2364755265.html","2a4249719045e677c361213abf0cabbf"],["/posts/2414116852.html","3096eac30a11e8541a33da6ddb62d6eb"],["/posts/2421785022.html","a389a7cf9270f0be127f173389f29ca0"],["/posts/2482902029.html","8c7c854228985b32432822b5fa9d843c"],["/posts/2495386210.html","dfdec641415ab7a74874eadedfb5f201"],["/posts/2516528882.html","66044518f3c3f166c12f7b8738b7be51"],["/posts/2526659543.html","f9d9010aad00d6fd92931f27ad3f3a3d"],["/posts/2529807823.html","e421dc75ad25c09dafe41257e8dc1bb7"],["/posts/2596601004.html","baeb2e9d412196c54537e438448b383a"],["/posts/2742438348.html","9c468a4630eecece0aea851444befbd6"],["/posts/2864584994.html","025006dd52483e8dbe6fa462c3cffb2c"],["/posts/2888309600.html","85b8406021e60213a0e85f5ec96da51e"],["/posts/2891591958.html","d14acc96525821bdbbc13d413e77f4e4"],["/posts/2909934084.html","4941cec1a682c85b23503d2b92e3f564"],["/posts/2920256992.html","2ad764e99578b4421a6be2d52232a0a6"],["/posts/2959474469.html","9257e6a0698ba4dc8f447499e16d1607"],["/posts/3005926051.html","c12fde441ec9b6e05aecdf116b6fa35d"],["/posts/309775400.html","891d4e1dbcd75b7b66008788619f3836"],["/posts/3156194925.html","98436b8bfdb15d0d26aa3905bbd89f4e"],["/posts/3169224211.html","8d725f48af9c16032c9712dcd2d27a58"],["/posts/3213899550.html","3a7d6e3c18dff1159a2ad1736dd35141"],["/posts/3259212833.html","b6994832236efc58b8733c56c36877f5"],["/posts/3266130344.html","522cc3f9ec793933c29ed8582e977b16"],["/posts/3292663995.html","09dbb295f40fb9461892095ce370e702"],["/posts/3297135020.html","c06ad63b16a9edcfc56a1035d7b7825d"],["/posts/3306641566.html","7ad9c11b0efcc408e3dcc9dd5ecdde45"],["/posts/3312011324.html","b29448fbb609bc7e3716d327b772ce6d"],["/posts/336911618.html","f6073c284d30638ef1121773d574fe6e"],["/posts/3402121571.html","a11afb70b3aa6f07a57237483e28ebe7"],["/posts/3405577485.html","ca38b812c955b745d4bbe85c7b269590"],["/posts/3498516849.html","7fa94a6426ced1e076aad2243407814b"],["/posts/3513711414.html","abfbf4613e9cfe7f40877a18aa97f7b2"],["/posts/3523095624.html","97c52eba17746d625b1e16c9f7fb8f4c"],["/posts/3546711884.html","b95b398ddf32c8c7cc1a0cb07c1e4f30"],["/posts/3731385230.html","f10ee2ee8c75b7887e71bf11b3d26823"],["/posts/3772089482.html","1386243781cc79f97f2bccd4af1e5bc5"],["/posts/386609427.html","b8814d82d3749e217b2037fe45850d3b"],["/posts/4044235327.html","18a470cccdf68656928881ee965973fb"],["/posts/4115971639.html","150c4f6599496472799ff483cbffacb4"],["/posts/4130790367.html","40e924a6294e14f8257f399c913c97de"],["/posts/4131986683.html","0d75dc7fd0eb55cce36aeac73a49e657"],["/posts/4177218757.html","3b58414ad2e2be587c840cc80638efe6"],["/posts/4192183953.html","f78628f2efbbd8585cec3726ba7680e4"],["/posts/4261103898.html","4b4c8cb22974ca90297c10d2ea9934b5"],["/posts/469711973.html","89ca253c599d7b857526639afa16fd37"],["/posts/482495853.html","2e744de7ffb6980da4ec2fd6793489da"],["/posts/488247922.html","148109ce5999f0a0a0cebdbb3a9edda8"],["/posts/517302816.html","fdef93efb464624884e95ac86dd00e00"],["/posts/570165348.html","60dff40adfbbff4c816a24436a7f5637"],["/posts/595890772.html","c971818421b924ca29d7561ea9b30d33"],["/posts/67485572.html","36bd0cdb46876a1a4adea57aca57f6b9"],["/posts/694347442.html","9b8cd6f15a1d00cd4ac9245e78e774e1"],["/posts/707384687.html","b99bb372ed13763a1f0990ffb8630060"],["/posts/71180092.html","a28a811c001bd0a62e90c83fff136881"],["/posts/716459272.html","449d57dfb0fed2208a422523f5fa0d61"],["/posts/765481613.html","0d41868f018f9dfeef295db747d85224"],["/posts/778231993.html","a550529cd48757b53464ad193c1b229e"],["/posts/795397410.html","4e9f0e5257ec840c110adf42ffe14684"],["/posts/820223701.html","f067471ab4eeb04677dd2d2725146cf9"],["/posts/830372185.html","a5ebebc329d78609ea13e393831cae4b"],["/posts/88294277.html","d428d56f8014bd3c300ff25126e3ab43"],["/posts/939963535.html","33f3dca71d87a626cde65a53cc2d1c52"],["/posts/983786067.html","483629536a5d6d82ad2da3f961f52946"],["/sw-register.js","8c79214ff1d180770145c97b3f7baad7"],["/tags/C/index.html","522ebce4d9c9465ff493f49ff7825b36"],["/tags/C/page/2/index.html","26e283e69dad223b98eb1d6ce1e8fc35"],["/tags/C/page/3/index.html","448701c748a0985beba41349c860750c"],["/tags/ETL/index.html","77ffd603b1711af7ce2d2da3c633b623"],["/tags/ElasticSearch/index.html","206fc0eade98d0877c35d03074160d9b"],["/tags/GUI/index.html","824c9712ff9a26bf148e38691652c98f"],["/tags/HBase/index.html","0b83724ea7ca16f8eebbd2ec4e2119dd"],["/tags/Hadoop/index.html","df3f7af6bd00c5f6c5b540a68284c347"],["/tags/Hadoop/page/2/index.html","ffcc97fa2cb2b71506cd24e9a8923b5d"],["/tags/Java/index.html","e62fe8550d2e6980a7ad2c4435c649a8"],["/tags/Java后端/index.html","b39ee5e804b0561752d8c77d9f95edad"],["/tags/Java后端/page/2/index.html","43307aa233f1047fa13c6c135a920c57"],["/tags/Java基础/index.html","a44c93aed8fc28555630a4fae089b9d4"],["/tags/Java基础/page/2/index.html","d561a19826db0a4fd428a7c65d938ff7"],["/tags/Kettle/index.html","315e67d46ace40f70308668429f33979"],["/tags/Kibana/index.html","86198f15d6be08386f4565437b6bd43d"],["/tags/Linux/index.html","bb28e59647ed983893446e06e51baf54"],["/tags/Linux/page/2/index.html","9ee1159599ac554b3e821ede066a9716"],["/tags/Linux/page/3/index.html","3d769bd5d2b8a140aecfa0a02fcb6df1"],["/tags/Mac/index.html","5f10a89cd4f05b47a4b7eaabd24fcd64"],["/tags/Mac/page/2/index.html","22d662d8a788b22d1a07cf4d694edfe2"],["/tags/Maven/index.html","0775f9b1655427130f1c7d5df1eb37d7"],["/tags/MySQL/index.html","a371936d085f523bbf5f3e55312d13e9"],["/tags/Python/index.html","a0e5ecf7062340e13a92c1bfdcc26f20"],["/tags/Redis/index.html","3309d28d94e2b3f896e636bb5c95694c"],["/tags/R语言/index.html","ac3a4cbd8c951f61b7561c093a820c74"],["/tags/Spark/index.html","72e655a474ae8198882524a742cb5931"],["/tags/Ubuntu/index.html","874aeafefee0c6e28b4b4335c17a415e"],["/tags/Vue/index.html","f27c6de489d6d59d53536fa658b857f1"],["/tags/Windows/index.html","156014b36249918fe7e9a1618bda3bd3"],["/tags/ZooKeeper/index.html","22ad3738df59557d1b27a58bcd944dfa"],["/tags/bfs/index.html","5b603a25e21fdeb50fbc829f60f8d6f3"],["/tags/dfs/index.html","e24833f3fbeaf3109d328158cb196937"],["/tags/folium/index.html","1724da7f8af645dc33dda08fb9039fa9"],["/tags/git/index.html","b2d79f37cfd0936a681e39f5c07a8f7c"],["/tags/index.html","18aa2437dca781b1ae4527c97a253538"],["/tags/latex/index.html","8d4e65a83e0116165ad8cd86b4d20b54"],["/tags/中间件/index.html","21bcfd62030cb7b8588d5c9f89f92572"],["/tags/二分查找/index.html","169ecec3863bb03bba7b66a39a676008"],["/tags/优化类/index.html","2d18e4976e4f3cc8ef931dfaa07e5654"],["/tags/前端/index.html","3bd84c8a93318922c301bf6aff127245"],["/tags/前缀和与差分/index.html","0e01dce2746b321e3f37b33dece83d4c"],["/tags/动态规划/index.html","9ab14742907d0fa1ee5f5c17ca8fefcb"],["/tags/动态规划/page/2/index.html","14334f0218c340dc6f6b140cce338706"],["/tags/博客搭建/index.html","ff6b1e482f8f9def3d01d1f7d17dc91c"],["/tags/图论/index.html","60070d9555d7ed32ea669bfe18ce87d1"],["/tags/大数据/index.html","27601404a982b5a21f59302d2c113f75"],["/tags/大数据/page/2/index.html","27bf536ebd9e4e14f92498f008006e24"],["/tags/操作系统/index.html","a1df5f0c430857cdfd695be25edf44a3"],["/tags/数学建模/index.html","afc7ff073006a1efb9b6923dd02e0eaf"],["/tags/数据库/index.html","23e607475a3807912aa0de8370ea19c8"],["/tags/数据结构和算法/index.html","a8b235d2879b5fb1ec0d4b988c6779f3"],["/tags/数据结构和算法/page/2/index.html","25f243f602f6f9ce3ace49a7f63bfdc7"],["/tags/数据结构和算法/page/3/index.html","c2e2f2f50d1ed2ec6267d23a3470f1e8"],["/tags/数据结构和算法/page/4/index.html","d4f00f86376300fd18cbf46dc771dda4"],["/tags/数组和字符串/index.html","fabd7d0ca6b557214627621be7afdeb4"],["/tags/数论/index.html","37eebe9c79e33ae82045aca7846e43a2"],["/tags/枚举类/index.html","d7c7c0184e7e5cfe7fd8cd3a69f6031c"],["/tags/栈和队列/index.html","342a13deaf60eee740e62fadc13fa434"],["/tags/树论/index.html","e4def1e688bee2119526428d7c944e8a"],["/tags/测试/index.html","65473310559e960252774c21d6f72622"],["/tags/环境/index.html","4047011e325c87093423d0228496d82c"],["/tags/环境变量/index.html","fe793963f21815b4e89552901e0ad5a7"],["/tags/绘图/index.html","0a0986851de3d2bb3d42982a88ae4314"],["/tags/编程工具/index.html","2bd06c30e997519741b911d3e28f0d83"],["/tags/编程环境/index.html","ac1388f75e816172dff78989dd7ed03d"],["/tags/网络编程/index.html","a7294f3b7cfab786b961fc2edba24a37"],["/tags/英语语法/index.html","b1eeae8f7f1be12f2c798f594b482af6"],["/tags/计算机操作系统/index.html","90cb79161010475f4b13cae1b3023792"],["/tags/论文/index.html","c9c2e3d67e10989059e399ede6591c1f"],["/tags/资源下载/index.html","be87055fd4b310a8a1d35726efb7209d"],["/tags/链表/index.html","341a32f92dbfa0ff28e6b23123799668"],["/tags/集合/index.html","89264b5bb054d67c41911a3f484910d6"],["/tags/集群/index.html","28f656925505872842cb7e961072f6ee"]];
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
