(window["webpackJsonp"]=window["webpackJsonp"]||[]).push([[3],{3269:function(a){a.exports=JSON.parse('{"series":[{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"}],"dimensions":["Date","Calgary","Edmonton","North","Central","South"],"source":[{"Date":"2020-03-06","Calgary":1},{"Date":"2020-03-07","Calgary":1},{"Date":"2020-03-08","Calgary":1},{"Date":"2020-03-09","Calgary":4,"Edmonton":3},{"Date":"2020-03-10","Calgary":10,"Edmonton":6},{"Date":"2020-03-11","Calgary":15,"Central":1,"Edmonton":8},{"Date":"2020-03-12","Calgary":17,"Central":1,"Edmonton":8},{"Date":"2020-03-13","Calgary":25,"Central":1,"Edmonton":8},{"Date":"2020-03-14","Calgary":38,"Central":1,"Edmonton":14},{"Date":"2020-03-15","Calgary":47,"Central":1,"Edmonton":15},{"Date":"2020-03-16","Calgary":62,"Central":2,"Edmonton":20,"North":2,"South":1},{"Date":"2020-03-17","Calgary":68,"Central":3,"Edmonton":27,"North":3,"South":1},{"Date":"2020-03-18","Calgary":90,"Central":3,"Edmonton":30,"North":4,"South":3},{"Date":"2020-03-19","Calgary":103,"Central":3,"Edmonton":38,"North":11,"South":4},{"Date":"2020-03-20","Calgary":128,"Central":5,"Edmonton":51,"North":14,"South":6},{"Date":"2020-03-21","Calgary":153,"Central":7,"Edmonton":57,"North":16,"South":7},{"Date":"2020-03-22","Calgary":182,"Central":13,"Edmonton":63,"North":18,"South":8},{"Date":"2020-03-23","Calgary":208,"Central":26,"Edmonton":76,"North":18,"South":8},{"Date":"2020-03-24","Calgary":245,"Central":34,"Edmonton":90,"North":20,"South":10},{"Date":"2020-03-25","Calgary":293,"Central":36,"Edmonton":104,"North":22,"South":12},{"Date":"2020-03-26","Calgary":332,"Central":40,"Edmonton":109,"North":26,"South":12},{"Date":"2020-03-27","Calgary":372,"Central":48,"Edmonton":129,"North":42,"South":12},{"Date":"2020-03-28","Calgary":405,"Central":48,"Edmonton":146,"North":46,"South":12},{"Date":"2020-03-29","Calgary":425,"Central":49,"Edmonton":154,"North":46,"South":12},{"Date":"2020-03-30","Calgary":449,"Central":53,"Edmonton":174,"North":49,"South":12},{"Date":"2020-03-31","Calgary":532,"Central":57,"Edmonton":210,"North":51,"South":12},{"Date":"2020-04-01","Calgary":583,"Central":58,"Edmonton":244,"North":51,"South":13},{"Date":"2020-04-02","Calgary":589,"Central":59,"Edmonton":247,"North":55,"South":14}]}')},"8b24":function(a,e,n){"use strict";n.r(e);var t=function(){var a=this,e=a.$createElement,n=a._self._c||e;return n("q-page",{staticClass:"flex fit row wrap justify-center items-start content-start"},[n("div",{staticClass:"q-pa-md"},[n("div",{staticClass:"row"},[n("div",{staticClass:"col"},[n("h4",[a._v("Canada and other countries")]),n("p",{staticClass:"text-body1"},[a._v("\n          Compare the number of cases in Canada to data from other countries.\n        ")])])]),n("div",{staticClass:"row"},[n("div",{staticClass:"col"},[n("q-toggle",{attrs:{label:"Log scale","left-label":""},model:{value:a.logscale,callback:function(e){a.logscale=e},expression:"logscale"}})],1),n("div",{staticClass:"col"},[n("q-select",{staticStyle:{width:"250px"},attrs:{rounded:"",standout:"","use-input":"","use-chips":"",multiple:"",hint:"Select countries","input-debounce":"0",options:a.filterOptions},on:{"new-value":a.createValue,filter:a.filterFn},model:{value:a.model,callback:function(e){a.model=e},expression:"model"}})],1)]),n("div",{staticClass:"row"},[n("div",{staticClass:"col"},[n("v-chart",{attrs:{options:a.lineCountries}})],1)]),n("div",{staticClass:"row"},[n("div",{staticClass:"col"},[n("h4",[a._v("Canadian provinces and territories")]),n("p",{staticClass:"text-body1"},[a._v("\n          Compare provinces and territories within Canada.\n        ")])])]),n("div",{staticClass:"row"},[n("div",{staticClass:"col"},[n("v-chart",{attrs:{options:a.lineCanada}})],1)]),n("div",{staticClass:"row"},[n("div",{staticClass:"col"},[n("h4",[a._v("Zones and areas within Alberta")]),n("p",{staticClass:"text-body1"},[a._v("\n          Compare zones and areas within Alberta.\n        ")])])]),n("div",{staticClass:"row"},[n("div",{staticClass:"col"},[n("v-chart",{attrs:{options:a.lineAlberta}})],1)])])])},r=[],o=(n("6762"),n("9ca8")),i=(n("94b1"),n("ef97"),n("007d"),n("b11c"),n("0a6d"),n("d28f"),n("627c"),n("3269")),l=n("fe67"),s=n("ed07"),c=["France","Spain","Italy","Canada","Morocco","Hungary"],d={legend:{},tooltip:{},dataset:{dimensions:[],source:[]},xAxis:{type:"category"},yAxis:{type:"log"},series:[],dataZoom:[{type:"inside",start:0,end:100},{start:0,end:100,handleIcon:"M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z",handleSize:"80%",handleStyle:{color:"#fff",shadowBlur:3,shadowColor:"rgba(0, 0, 0, 0.6)",shadowOffsetX:2,shadowOffsetY:2}}],toolbox:{show:!0,orient:"vertical",feature:{dataZoom:{title:{zoom:"",back:""}},magicType:{type:["line","bar"],title:{line:"",bar:""}},restore:{},saveAsImage:{title:"Save"}}}};d.dataset.source=i.source,d.dataset.dimensions=i.dimensions,d.series=i.series;var u={legend:{},tooltip:{},dataset:{dimensions:[],source:[]},xAxis:{type:"category"},yAxis:{type:"log"},series:[],dataZoom:[{type:"inside",start:0,end:100},{start:0,end:100,handleIcon:"M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z",handleSize:"80%",handleStyle:{color:"#fff",shadowBlur:3,shadowColor:"rgba(0, 0, 0, 0.6)",shadowOffsetX:2,shadowOffsetY:2}}],toolbox:{show:!0,orient:"vertical",feature:{dataZoom:{title:{zoom:"",back:""}},magicType:{type:["line","bar"],title:{line:"",bar:""}},restore:{},saveAsImage:{title:"Save"}}}};u.dataset.source=l.source,u.dataset.dimensions=l.dimensions,u.series=l.series;var y={legend:{},tooltip:{},dataset:{dimensions:[],source:[]},xAxis:{type:"category"},yAxis:{type:"log"},series:[],dataZoom:[{type:"inside",start:0,end:100},{start:0,end:100,handleIcon:"M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z",handleSize:"80%",handleStyle:{color:"#fff",shadowBlur:3,shadowColor:"rgba(0, 0, 0, 0.6)",shadowOffsetX:2,shadowOffsetY:2}}],toolbox:{show:!0,orient:"vertical",feature:{dataZoom:{title:{zoom:"",back:""}},magicType:{type:["line","bar"],title:{line:"",bar:""}},restore:{},saveAsImage:{title:"Save"}}}};y.dataset.source=s.source,y.dataset.dimensions=s.dimensions,y.series=s.series;var C={name:"PageIndex",components:{"v-chart":o["a"]},data:function(){return{logscale:!0,model:null,filterOptions:c,lineCountries:y,lineCanada:u,lineAlberta:d}},methods:{createValue:function(a,e){a.length>0&&(c.includes(a)||c.push(a),e(a,"toggle"))},filterFn:function(a,e){var n=this;e((function(){if(""===a)n.filterOptions=c;else{var e=a.toLowerCase();n.filterOptions=c.filter((function(a){return a.toLowerCase().indexOf(e)>-1}))}}))}}},p=C,b=n("2877"),m=n("eebe"),h=n.n(m),D=n("9989"),w=n("9564"),S=n("ddd8"),g=Object(b["a"])(p,t,r,!1,null,null,null);e["default"]=g.exports;h()(g,"components",{QPage:D["a"],QToggle:w["a"],QSelect:S["a"]})},ed07:function(a){a.exports=JSON.parse('{"series":[{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"}],"dimensions":["Date","France","Spain","Italy","Japan","Canada","Germany","Morocco","Hungary"],"source":[{"Date":"2020-01-24","France":2},{"Date":"2020-01-25","France":3},{"Date":"2020-01-26","France":3,"Japan":4,"Canada":1},{"Date":"2020-01-27","France":3,"Japan":4,"Canada":1,"Germany":1},{"Date":"2020-01-28","France":4,"Japan":7,"Canada":2,"Germany":4},{"Date":"2020-01-29","France":5,"Japan":7,"Canada":2,"Germany":4},{"Date":"2020-01-30","France":5,"Japan":11,"Canada":2,"Germany":4},{"Date":"2020-01-31","France":5,"Italy":2,"Japan":15,"Canada":4,"Germany":5},{"Date":"2020-02-01","France":6,"Spain":1,"Italy":2,"Japan":20,"Canada":4,"Germany":8},{"Date":"2020-02-02","France":6,"Spain":1,"Italy":2,"Japan":20,"Canada":4,"Germany":10},{"Date":"2020-02-03","France":6,"Spain":1,"Italy":2,"Japan":20,"Canada":4,"Germany":12},{"Date":"2020-02-04","France":6,"Spain":1,"Italy":2,"Japan":22,"Canada":4,"Germany":12},{"Date":"2020-02-05","France":6,"Spain":1,"Italy":2,"Japan":22,"Canada":5,"Germany":12},{"Date":"2020-02-06","France":6,"Spain":1,"Italy":2,"Japan":22,"Canada":5,"Germany":12},{"Date":"2020-02-07","France":6,"Spain":1,"Italy":3,"Japan":25,"Canada":7,"Germany":13},{"Date":"2020-02-08","France":11,"Spain":1,"Italy":3,"Japan":25,"Canada":7,"Germany":13},{"Date":"2020-02-09","France":11,"Spain":2,"Italy":3,"Japan":26,"Canada":7,"Germany":14},{"Date":"2020-02-10","France":11,"Spain":2,"Italy":3,"Japan":26,"Canada":7,"Germany":14},{"Date":"2020-02-11","France":11,"Spain":2,"Italy":3,"Japan":26,"Canada":7,"Germany":16},{"Date":"2020-02-12","France":11,"Spain":2,"Italy":3,"Japan":28,"Canada":7,"Germany":16},{"Date":"2020-02-13","France":11,"Spain":2,"Italy":3,"Japan":28,"Canada":7,"Germany":16},{"Date":"2020-02-14","France":11,"Spain":2,"Italy":3,"Japan":29,"Canada":7,"Germany":16},{"Date":"2020-02-15","France":12,"Spain":2,"Italy":3,"Japan":43,"Canada":7,"Germany":16},{"Date":"2020-02-16","France":12,"Spain":2,"Italy":3,"Japan":59,"Canada":7,"Germany":16},{"Date":"2020-02-17","France":12,"Spain":2,"Italy":3,"Japan":66,"Canada":8,"Germany":16},{"Date":"2020-02-18","France":12,"Spain":2,"Italy":3,"Japan":74,"Canada":8,"Germany":16},{"Date":"2020-02-19","France":12,"Spain":2,"Italy":3,"Japan":84,"Canada":8,"Germany":16},{"Date":"2020-02-20","France":12,"Spain":2,"Italy":3,"Japan":94,"Canada":8,"Germany":16},{"Date":"2020-02-21","France":12,"Spain":2,"Italy":20,"Japan":105,"Canada":9,"Germany":16},{"Date":"2020-02-22","France":12,"Spain":2,"Italy":62,"Japan":122,"Canada":9,"Germany":16},{"Date":"2020-02-23","France":12,"Spain":2,"Italy":155,"Japan":147,"Canada":9,"Germany":16},{"Date":"2020-02-24","France":12,"Spain":2,"Italy":229,"Japan":159,"Canada":10,"Germany":16},{"Date":"2020-02-25","France":14,"Spain":6,"Italy":322,"Japan":170,"Canada":11,"Germany":17},{"Date":"2020-02-26","France":18,"Spain":13,"Italy":453,"Japan":189,"Canada":11,"Germany":27},{"Date":"2020-02-27","France":38,"Spain":15,"Italy":655,"Japan":214,"Canada":13,"Germany":46},{"Date":"2020-02-28","France":57,"Spain":32,"Italy":888,"Japan":228,"Canada":14,"Germany":48},{"Date":"2020-02-29","France":100,"Spain":45,"Italy":1128,"Japan":241,"Canada":20,"Germany":79},{"Date":"2020-03-01","France":130,"Spain":84,"Italy":1694,"Japan":256,"Canada":24,"Germany":130},{"Date":"2020-03-02","France":191,"Spain":120,"Italy":2036,"Japan":274,"Canada":27,"Germany":159,"Morocco":1},{"Date":"2020-03-03","France":204,"Spain":165,"Italy":2502,"Japan":293,"Canada":30,"Germany":196,"Morocco":1},{"Date":"2020-03-04","France":285,"Spain":222,"Italy":3089,"Japan":331,"Canada":33,"Germany":262,"Morocco":1,"Hungary":2},{"Date":"2020-03-05","France":377,"Spain":259,"Italy":3858,"Japan":360,"Canada":37,"Germany":482,"Morocco":2,"Hungary":2},{"Date":"2020-03-06","France":653,"Spain":400,"Italy":4636,"Japan":420,"Canada":49,"Germany":670,"Morocco":2,"Hungary":2},{"Date":"2020-03-07","France":949,"Spain":500,"Italy":5883,"Japan":461,"Canada":54,"Germany":799,"Morocco":2,"Hungary":4},{"Date":"2020-03-08","France":1126,"Spain":673,"Italy":7375,"Japan":502,"Canada":64,"Germany":1040,"Morocco":2,"Hungary":7},{"Date":"2020-03-09","France":1209,"Spain":1073,"Italy":9172,"Japan":511,"Canada":77,"Germany":1176,"Morocco":2,"Hungary":9},{"Date":"2020-03-10","France":1784,"Spain":1695,"Italy":10149,"Japan":581,"Canada":79,"Germany":1457,"Morocco":3,"Hungary":9},{"Date":"2020-03-11","France":2281,"Spain":2277,"Italy":12462,"Japan":639,"Canada":108,"Germany":1908,"Morocco":5,"Hungary":13},{"Date":"2020-03-12","France":2281,"Spain":2277,"Italy":12462,"Japan":639,"Canada":117,"Germany":2078,"Morocco":6,"Hungary":13},{"Date":"2020-03-13","France":3661,"Spain":5232,"Italy":17660,"Japan":701,"Canada":193,"Germany":3675,"Morocco":7,"Hungary":19},{"Date":"2020-03-14","France":4469,"Spain":6391,"Italy":21157,"Japan":773,"Canada":198,"Germany":4585,"Morocco":17,"Hungary":30},{"Date":"2020-03-15","France":4499,"Spain":7798,"Italy":24747,"Japan":839,"Canada":252,"Germany":5795,"Morocco":28,"Hungary":32},{"Date":"2020-03-16","France":6633,"Spain":9942,"Italy":27980,"Japan":839,"Canada":415,"Germany":7272,"Morocco":29,"Hungary":39},{"Date":"2020-03-17","France":7652,"Spain":11748,"Italy":31506,"Japan":878,"Canada":478,"Germany":9257,"Morocco":38,"Hungary":50},{"Date":"2020-03-18","France":9043,"Spain":13910,"Italy":35713,"Japan":889,"Canada":657,"Germany":12327,"Morocco":49,"Hungary":58},{"Date":"2020-03-19","France":10871,"Spain":17963,"Italy":41035,"Japan":924,"Canada":800,"Germany":15320,"Morocco":63,"Hungary":73},{"Date":"2020-03-20","France":12612,"Spain":20410,"Italy":47021,"Japan":963,"Canada":943,"Germany":19848,"Morocco":77,"Hungary":85},{"Date":"2020-03-21","France":14282,"Spain":25374,"Italy":53578,"Japan":1007,"Canada":1277,"Germany":22213,"Morocco":96,"Hungary":103},{"Date":"2020-03-22","France":16018,"Spain":28768,"Italy":59138,"Japan":1101,"Canada":1469,"Germany":24873,"Morocco":115,"Hungary":131},{"Date":"2020-03-23","France":19856,"Spain":35136,"Italy":63927,"Japan":1128,"Canada":2088,"Germany":29056,"Morocco":143,"Hungary":167},{"Date":"2020-03-24","France":22304,"Spain":39885,"Italy":69176,"Japan":1193,"Canada":2790,"Germany":32986,"Morocco":170,"Hungary":187},{"Date":"2020-03-25","France":25233,"Spain":49515,"Italy":74386,"Japan":1307,"Canada":3251,"Germany":37323,"Morocco":225,"Hungary":226},{"Date":"2020-03-26","France":29155,"Spain":57786,"Italy":80589,"Japan":1387,"Canada":4042,"Germany":43938,"Morocco":275,"Hungary":261},{"Date":"2020-03-27","France":32964,"Spain":65719,"Italy":86498,"Japan":1468,"Canada":4682,"Germany":50871,"Morocco":345,"Hungary":300},{"Date":"2020-03-28","France":37575,"Spain":73235,"Italy":92472,"Japan":1693,"Canada":5576,"Germany":57695,"Morocco":402,"Hungary":343},{"Date":"2020-03-29","France":40174,"Spain":80110,"Italy":97689,"Japan":1866,"Canada":6280,"Germany":62095,"Morocco":479,"Hungary":408},{"Date":"2020-03-30","France":44550,"Spain":87956,"Italy":101739,"Japan":1866,"Canada":7398,"Germany":66885,"Morocco":556,"Hungary":447},{"Date":"2020-03-31","France":52128,"Spain":95923,"Italy":105792,"Japan":1953,"Canada":8527,"Germany":71808,"Morocco":617,"Hungary":492},{"Date":"2020-04-01","France":56989,"Spain":104118,"Italy":110574,"Japan":2178,"Canada":9560,"Germany":77872,"Morocco":654,"Hungary":525},{"Date":"2020-04-02","France":59105,"Spain":112065,"Italy":115242,"Japan":2495,"Canada":11284,"Germany":84794,"Morocco":708,"Hungary":585}]}')},fe67:function(a){a.exports=JSON.parse('{"series":[{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"},{"type":"line"}],"dimensions":["Date","Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador","Nova Scotia","Ontario","Prince Edward Island","Quebec","Saskatchewan","Northwest Territories","Yukon"],"source":[{"Date":"2020-01-26","Ontario":1},{"Date":"2020-01-27","Ontario":1},{"Date":"2020-01-28","British Columbia":1,"Ontario":1},{"Date":"2020-01-29","British Columbia":1,"Ontario":1},{"Date":"2020-01-30","British Columbia":1,"Ontario":1},{"Date":"2020-01-31","British Columbia":1,"Ontario":3},{"Date":"2020-02-01","British Columbia":1,"Ontario":3},{"Date":"2020-02-02","British Columbia":1,"Ontario":3},{"Date":"2020-02-03","British Columbia":1,"Ontario":3},{"Date":"2020-02-04","British Columbia":1,"Ontario":3},{"Date":"2020-02-05","British Columbia":2,"Ontario":3},{"Date":"2020-02-06","British Columbia":2,"Ontario":3},{"Date":"2020-02-07","British Columbia":4,"Ontario":3},{"Date":"2020-02-08","British Columbia":4,"Ontario":3},{"Date":"2020-02-09","British Columbia":4,"Ontario":3},{"Date":"2020-02-10","British Columbia":4,"Ontario":3},{"Date":"2020-02-11","British Columbia":4,"Ontario":3},{"Date":"2020-02-12","British Columbia":4,"Ontario":3},{"Date":"2020-02-13","British Columbia":4,"Ontario":3},{"Date":"2020-02-14","British Columbia":4,"Ontario":3},{"Date":"2020-02-15","British Columbia":4,"Ontario":3},{"Date":"2020-02-16","British Columbia":4,"Ontario":3},{"Date":"2020-02-17","British Columbia":5,"Ontario":3},{"Date":"2020-02-18","British Columbia":5,"Ontario":3},{"Date":"2020-02-19","British Columbia":5,"Ontario":3},{"Date":"2020-02-20","British Columbia":5,"Ontario":3},{"Date":"2020-02-21","British Columbia":6,"Ontario":3},{"Date":"2020-02-22","British Columbia":6,"Ontario":3},{"Date":"2020-02-23","British Columbia":6,"Ontario":3},{"Date":"2020-02-24","British Columbia":6,"Ontario":4},{"Date":"2020-02-25","British Columbia":7,"Ontario":4},{"Date":"2020-02-26","British Columbia":7,"Ontario":4},{"Date":"2020-02-27","British Columbia":7,"Ontario":6},{"Date":"2020-02-28","British Columbia":7,"Ontario":6,"Quebec":1},{"Date":"2020-02-29","British Columbia":8,"Ontario":11,"Quebec":1},{"Date":"2020-03-01","British Columbia":8,"Ontario":15,"Quebec":1},{"Date":"2020-03-02","British Columbia":8,"Ontario":18,"Quebec":1},{"Date":"2020-03-03","British Columbia":9,"Ontario":20,"Quebec":1},{"Date":"2020-03-04","British Columbia":12,"Ontario":20,"Quebec":1},{"Date":"2020-03-05","British Columbia":13,"Ontario":22,"Quebec":2},{"Date":"2020-03-06","Alberta":1,"British Columbia":21,"Ontario":25,"Quebec":2},{"Date":"2020-03-07","Alberta":2,"British Columbia":21,"Ontario":28,"Quebec":3},{"Date":"2020-03-08","Alberta":4,"British Columbia":27,"Ontario":29,"Quebec":4},{"Date":"2020-03-09","Alberta":7,"British Columbia":32,"Ontario":34,"Quebec":4},{"Date":"2020-03-10","Alberta":7,"British Columbia":32,"Ontario":36,"Quebec":4},{"Date":"2020-03-11","Alberta":19,"British Columbia":39,"New Brunswick":1,"Ontario":41,"Quebec":8},{"Date":"2020-03-12","Alberta":19,"British Columbia":46,"New Brunswick":1,"Ontario":42,"Quebec":9},{"Date":"2020-03-13","Alberta":29,"British Columbia":64,"Manitoba":4,"New Brunswick":1,"Ontario":74,"Quebec":17,"Saskatchewan":2},{"Date":"2020-03-14","Alberta":29,"British Columbia":64,"Manitoba":4,"New Brunswick":1,"Ontario":79,"Quebec":17,"Saskatchewan":2},{"Date":"2020-03-15","Alberta":39,"British Columbia":73,"Manitoba":4,"New Brunswick":2,"Newfoundland and Labrador":1,"Ontario":104,"Prince Edward Island":1,"Quebec":24,"Saskatchewan":2},{"Date":"2020-03-16","Alberta":56,"British Columbia":103,"Manitoba":7,"New Brunswick":6,"Newfoundland and Labrador":1,"Nova Scotia":5,"Ontario":177,"Prince Edward Island":1,"Quebec":50,"Saskatchewan":7},{"Date":"2020-03-17","Alberta":74,"British Columbia":103,"Manitoba":8,"New Brunswick":8,"Newfoundland and Labrador":3,"Nova Scotia":7,"Ontario":185,"Prince Edward Island":1,"Quebec":74,"Saskatchewan":7},{"Date":"2020-03-18","Alberta":97,"British Columbia":186,"Manitoba":15,"New Brunswick":11,"Newfoundland and Labrador":3,"Nova Scotia":12,"Ontario":221,"Prince Edward Island":1,"Quebec":94,"Saskatchewan":8},{"Date":"2020-03-19","Alberta":119,"British Columbia":231,"Manitoba":17,"New Brunswick":11,"Newfoundland and Labrador":3,"Nova Scotia":14,"Ontario":257,"Prince Edward Island":2,"Quebec":121,"Saskatchewan":16},{"Date":"2020-03-20","Alberta":146,"British Columbia":271,"Manitoba":17,"New Brunswick":11,"Newfoundland and Labrador":4,"Nova Scotia":15,"Ontario":308,"Prince Edward Island":2,"Quebec":139,"Saskatchewan":20},{"Date":"2020-03-21","Alberta":195,"British Columbia":424,"Manitoba":18,"New Brunswick":17,"Newfoundland and Labrador":6,"Nova Scotia":21,"Ontario":377,"Prince Edward Island":2,"Quebec":181,"Saskatchewan":26},{"Date":"2020-03-22","Alberta":259,"British Columbia":424,"Manitoba":20,"New Brunswick":17,"Newfoundland and Labrador":9,"Nova Scotia":28,"Ontario":425,"Prince Edward Island":3,"Quebec":219,"Saskatchewan":52},{"Date":"2020-03-23","Alberta":301,"British Columbia":472,"Manitoba":20,"New Brunswick":17,"Newfoundland and Labrador":24,"Nova Scotia":41,"Ontario":503,"Prince Edward Island":3,"Quebec":628,"Saskatchewan":66},{"Date":"2020-03-24","Alberta":359,"British Columbia":617,"Manitoba":21,"New Brunswick":18,"Newfoundland and Labrador":35,"Nova Scotia":51,"Ontario":588,"Prince Edward Island":3,"Quebec":1013,"Saskatchewan":72},{"Date":"2020-03-25","Alberta":358,"British Columbia":617,"Manitoba":35,"New Brunswick":18,"Newfoundland and Labrador":35,"Nova Scotia":68,"Ontario":688,"Prince Edward Island":5,"Quebec":1342,"Saskatchewan":72},{"Date":"2020-03-26","Alberta":486,"British Columbia":725,"Manitoba":36,"New Brunswick":33,"Newfoundland and Labrador":82,"Nova Scotia":73,"Ontario":858,"Prince Edward Island":5,"Quebec":1632,"Saskatchewan":95,"Northwest Territories":1,"Yukon":3},{"Date":"2020-03-27","Alberta":542,"British Columbia":725,"Manitoba":39,"New Brunswick":45,"Newfoundland and Labrador":102,"Nova Scotia":90,"Ontario":994,"Prince Edward Island":9,"Quebec":2024,"Saskatchewan":95,"Northwest Territories":1,"Yukon":3},{"Date":"2020-03-28","Alberta":542,"British Columbia":884,"Manitoba":64,"New Brunswick":51,"Newfoundland and Labrador":120,"Nova Scotia":110,"Ontario":1144,"Prince Edward Island":11,"Quebec":2498,"Saskatchewan":134,"Northwest Territories":1,"Yukon":4},{"Date":"2020-03-29","Alberta":621,"British Columbia":884,"Manitoba":72,"New Brunswick":66,"Newfoundland and Labrador":135,"Nova Scotia":122,"Ontario":1355,"Prince Edward Island":11,"Quebec":2840,"Saskatchewan":156,"Northwest Territories":1,"Yukon":4},{"Date":"2020-03-30","Alberta":661,"British Columbia":970,"Manitoba":96,"New Brunswick":68,"Newfoundland and Labrador":148,"Nova Scotia":127,"Ontario":1706,"Prince Edward Island":18,"Quebec":3430,"Saskatchewan":156,"Northwest Territories":1,"Yukon":4},{"Date":"2020-03-31","Alberta":690,"British Columbia":1013,"Manitoba":103,"New Brunswick":70,"Newfoundland and Labrador":152,"Nova Scotia":147,"Ontario":1966,"Prince Edward Island":21,"Quebec":4162,"Saskatchewan":184,"Northwest Territories":1,"Yukon":5},{"Date":"2020-04-01","Alberta":754,"British Columbia":1013,"Manitoba":127,"New Brunswick":81,"Newfoundland and Labrador":175,"Nova Scotia":173,"Ontario":2392,"Prince Edward Island":21,"Quebec":4611,"Saskatchewan":193,"Northwest Territories":2,"Yukon":5},{"Date":"2020-04-02","Alberta":969,"British Columbia":1121,"Manitoba":167,"New Brunswick":91,"Newfoundland and Labrador":183,"Nova Scotia":193,"Ontario":2793,"Prince Edward Island":22,"Quebec":5518,"Saskatchewan":206,"Northwest Territories":2,"Yukon":6}]}')}}]);