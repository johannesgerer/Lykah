
var mn, titleSpans = ".menu .visible.fixed div.title > span";
var widths = {};
var collapsed = false;
var collapsedEffect = false;
var currentTit = undefined;


function calculatedTop(ob){
  return 15 + $(ob).offset().top
       - mn.children("div").outerHeight()
}

function showTitle()
{
  var tit=undefined;
  $("#contentBody h1.title, .title[data-title]").each(function(){
    var s = $(window).scrollTop(),
        t = calculatedTop(this);
    if(s > t)
      tit = $(this)
    else if (t < s + 15)
      tit=undefined;
    else
      return false;
  })

  if(tit){
    currentTit = tit
    
    $(titleSpans).html(tit.data('title') ||
                       tit.contents().first().text())

    if(collapsedEffect){
      $(".menu .visible.fixed div.collapsedExtra.title")
        .css({ float:'none' ,display:'block' })
    }else{
      $(".visible div.collapsedExtra.title").css({ float:'left'
                                                 , display:'none'
      })
    }
  }else{
    $(titleSpans).empty()
  }
}

function renderMenu(collapse, leaveFixStateOnUncollapse){
  $(".menu .hidden a.inactive").each(function(index){
    widths[index] = $(this).width();
  })
  if(collapse){
    if(!collapsed){
      collapsed = true
      mn.find("a.inactive").each(function(index){
        $(this).width(widths[index])
        $(this).width(0)
      })
      mn.addClass("fixed");
      mn.addClass("collapsed");
      window.setTimeout(function(){
        collapsedEffect = window.collapsed
        if(collapsedEffect)
          $(".menu .visible.fixed a.inactive").toggle(false)
        else{
          mn.find("a.inactive").each(function(index){
            $(this).width("")
          })
        }

        showTitle()
      }, 500)
    }
  } else {
    if(collapsed){
      $(".menu .visible.fixed a.inactive").toggle(true)
      mn.find("a.inactive").each(function(index){
        $(this).width(widths[index])
      })
      collapsed = false
      window.setTimeout(function(){
        collapsedEffect = window.collapsed
        if(!collapsedEffect)
          mn.find("a.inactive").each(function(index){
            $(this).width("")
          })
      }, 500)
      if(!leaveFixStateOnUncollapse){
        mn.removeClass("fixed");
      }
      mn.removeClass("collapsed");
    }
    collapsedEffect = window.collapsed
  }
  showTitle()
}

function onscroll() {
  renderMenu( $(window).scrollTop() > $(".menu .hidden").offset().top, false)
}

$(window).load(function(){
  /* retinajs()*/
  ga('require', 'displayfeatures');ga('send', 'pageview');
  
  mn = $(".menu .visible");

  $(".menu .visible div.title > span").click(function()
    {
      if(currentTit){
        renderMenu(true, true)
        window.setTimeout(function(){
          $(window).scrollTop(calculatedTop(currentTit) - 3)
        },200)
        return false;
      }
    })
  
  $(".menu").click(function(e){
    e.stopPropagation()
  })
  
  $(window).click(function(){
    onscroll()
  })
  
  $(".menu div.collapsedExtra.hamburger ").click(function()
    {
      renderMenu(false, true)
      return false
    })
  
  $(window).scroll(onscroll).resize(onscroll)

  onscroll()
})
