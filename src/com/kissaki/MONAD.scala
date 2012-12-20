class MONAD {
	//covariant - parameterizedType Aをもつ、ケースクラス A
	case class Thing[+A](value: A)

	val a = Thing(1)
	val b = Thing("文字です")
	println("a	"+a)
	println("b	"+b)


	//int1つを取って、Thingに1+iを入れたオブジェクトを返す
	def   foo(i: Int) = Thing(i + 1)
 
	val a2 = Thing(1)
	val b2 = foo(a2.value)        // => Thing(2)


	//Things2は、Thing2 から値を取り出し、その値を用いて別の関数を呼び出し、呼び出しのその戻り値（新たな Thing2）を返すという関数
	case class Thing2[+A](value: A) {

		/**
			f2という引数(Aを引数にとってThing2を返す関数)を実行する関数。
			具体的には、こう書くことで、型Thing2になんらかの型タイプを入力しつつ、その関数からThing2型のオブジェクトを返すことができる。
			ひとつのオブジェクトをガンガン掘って独自情報溜めることができるようになる。
		*/
	  def myBind[B](f2: A => Thing2[B]) = f2(value)
	}

	//Thing2を返すメソッドfoo2を作る。
	def   foo2(i: Int) = Thing2(i + 1)

	//cはThing2
	val c = Thing2(1)

	//dはcに対してmyBindメソッドで「foo2というメソッド」を渡したあとの値
	val d = c.myBind(foo2)
	println("d	"+d)

	/*
		処理順を追ってみよう。
		まず、val　cは　Thing2(1)で作られる。
				この時、
				case class Thing2[+Int](value: Int) {
				  def myBind[B](f2: Int => Thing2[B]) = f2(value)
				}

				みたいな感じに定義される。

		で。
		val d = c.myBind(foo2)
		def   foo2(i: Int) = Thing2(i + 1)
				この時、
				cに対してcのmyBind関数が行うのは、
				関数fooを取り込んで、
				def myBind = 関数foo(value)　なので、
				返り値は
				Thing2((1) + 1) = Thing2(2) になる。

				
	*/

	//これは2回やってるので、+1 +1 で３になる。
	val e = c.myBind(foo2).myBind(foo2)
	println("e	"+e)

	println("この時点でcは	"+c)//cは作ったときのまま、変化しない

	val a3 = Thing2(1)//１が入った　＝Thing2(1)

	def aassa = {i:Int => Thing2(i + 1)}

	val a4 = a3.myBind(i => Thing2(i + 1))
	println("a4	"+a4)


}
val cMONAD = new MONAD

class FN {
	def firstName(id: Int): String = "t"    //　データベースから取得
	def lastName(id: Int): String = "i"
	 
	def fullName(id: Int): String = {
	  val fname = firstName(id)
	  if (fname != null) {
	    val lname = lastName(id)
	    if (lname != null)
	      fname + " " + lname
	    else
	      null
	  } else {
	    null
	  }
	}

	val result = fullName(0)
	println("FN	result	"+result)
}
new FN //t i

//こいつから、monadを探し出す。
/*
	→同じようなifがあったり、
	チェーンしてたり、っていうところなんでしょうね。

	たしかに、こういうのをifを使わずに扱えるとすっごく楽です。
*/


/*
	やってみよう。
	Thingで囲む
*/
class myFN {
	case class Thing[+A](value: A) {
	  def bind[B](f: A => Thing[B]) = f(value)
	}

	def firstName(id: Int): Thing[String] = Thing("t")
	def lastName(id: Int): Thing[String] = Thing("i")
	 
	def fullName(id: Int): Thing[String] = {
	  firstName(id).bind {//まず最初のThingをバインド、関数{}を与える
	  	fname => if (fname != null) {
	  		lastName(id).bind{
	  			lname => if (lname != null) {
	  				Thing(fname + lname)
	  			} else {
	  				Thing(null)
	  			}
	  		}
	  	} else {
	  		Thing(null)
	  	}
	  }
	}

	//まず、こんな感じにできる。
	/*
		やってる事は、fnameがもしあれば、その中からThingを返す。
		で、Thingを返すってことは、その部分は、bindで出力できる、ってことなので、
	*/
	def fullName2(id: Int): Thing[String] = {
	  val fname = firstName(id)
	  if (fname != null) {
	  	Thing("適当なThingを返す")//なかみぶっこぬいた。
	  } else {
	    Thing(null)
	  }
	}

	//で、
	/*
		bindをつかって、Thingで置き換えた中身を補ってみる。
		lnameはThingなので、bindに置き換えられる。

		def s(lnameIn:String) =  if (lname != null) Thing(fname + lnameIn) else Thing(null)
		っていう関数　= 文字列lnameをとって、Thingを返す関数
		を、val lname （こいつはThingになる）に、bindで渡す。

		おおー。
	*/
	def fullName3(id: Int): Thing[String] = {
	  val fname = firstName(id)
	  if (fname != null) {

	  	val lname = lastName(id)//こいつは、idに応じたlnameが入ってるThing。 関数をうけとったら、その関数にlnameの値を反映したものを返してくる。

	  	//中身を関数化して
	  	def s(lnameIn:String) =  if (lnameIn != null) Thing(fname + lnameIn) else Thing(null)

	  	//渡す
	  	lname.bind(s)

	  } else {
	    Thing(null)
	  }
	}


	// さらに、def s の部分は変形させる事が出来る。
	/**
		def s(lnameIn:String) =  if (lnameIn != null) Thing(fname + lnameIn) else Thing(null)
		は、無名関数
		val s = (lnameIn:String) => if (lnameIn != null) Thing(fname + lnameIn) else Thing(null)

		に書き換えられる。
	*/
	def fullName4(id: Int): Thing[String] = {
	  val fname = firstName(id)
	  if (fname != null) {
			val lname = lastName(id)

			//anonymousな関数に変形
			val s = (lnameIn:String) => if (lnameIn != null) Thing(fname + lnameIn) else Thing(null)

			lname.bind(s)

	  } else {
	    Thing(null)
	  }
	}

	//ぽいっと。
	/*
		取り替え。
	*/
	def fullName5(id: Int): Thing[String] = {
	  val fname = firstName(id)
	  if (fname != null) {
			lastName(id).bind(
				(lnameIn:String) => if (lnameIn != null) Thing(fname + lnameIn) else Thing(null)
			)
	  } else {
	    Thing(null)
	  }
	}


	//で。省略をかける
	/**
		型推論が効くので、型を取っ払える。
	*/
	def fullName6(id: Int): Thing[String] = {
	  val fname = firstName(id)
	  if (fname != null) {
			lastName(id).bind(lnameIn =>
				if (lnameIn != null) Thing(fname + lnameIn) else Thing(null)
			)
	  } else {
	    Thing(null)
	  }
	}

	//さらに、fnameのほうも同じ要領でいじってみる。
	/**
		畳む。
		という感じで、ここまでくる、と。
		Thingで全部包んでるから簡単にここまでは来れる。
	*/
	def fullName7(id: Int): Thing[String] = {
	  firstName(id).bind(fnameIn =>
		  if (fnameIn != null) {
				lastName(id).bind(lnameIn =>
					if (lnameIn != null) Thing(fnameIn + lnameIn) else Thing(null)
				)
		  } else {
		    Thing(null)
		  }
	  )
	}

	//で、ifウザいですねってなってくる。この辺から型を使った世界へ
	/**
		traitを定義、最終目的は、ifを「型マッチで」消すこと。
	*/
	//基礎になる、オレオレ MyOption
	trait MyOption[+A] {//Nothingがすべての型を継承しているため、MyNoneでNothing を使いたいなら、co-variantな必要がある。
	  def bind[B](f: A => MyOption[B]): MyOption[B]
	}

	/**
		まずはThingを改造、値があったときに使う型を定義してみる。
		引数が、なんか値を返すような関数なら、MyOptionを使った際、このThing2が実行される。
	*/
	case class Thing2[+A](value: A) extends MyOption[A] {//親の型は、このMySomeが使われるときの型と同じ。
		def bind[B](f: A => MyOption[B]) = {
	  	f(value)//関数を実行、 MyOption 自体を返す。 MyOption がcoVariantなので、可能。
	  }
	}
	/*
		と、値が無かったときのためのものを定義してみる。
		引数となる関数がそも引数がNothingだったら、このMyNoneを返す。
	*/
	case object MyNone extends MyOption[Nothing] {//親の型を、Nothingに指定
	  def bind[B](f: Nothing => MyOption[B]) = MyNone//関数を実行すると、MyNoneというオブジェクトが帰ってくる。
	}


	// で。
	/*
		上記を使って、Thingを全面的に MyOption と書き換え。もちろん、firstNameとかも。
		ここでは、別途定義しちゃう。

		MyOptionはtraitなので、値のほうは、MySomeを使う。

		展開の順番としては、
		・Thingに、値があるか、無いかの判断をする機構を入れようぜ。
		・めんどくせーから型で解決しよう。
		・じゃあ、値が有るときのcase classを現状のThingからThing2にして、それと同じ親を継承したobject MyNoneっていうのを作ろう。
		・共通の親が必要なので、それはMyOptionって名前にしよう。
		・で、MyOptionは、継承する都合上、trait。
		・MyOptionを継承したThing2と、MyNoneが出来上がる。
		・MyNoneは、bindをオーバーロードした「Nothingを受け取って動く関数」を受け取ったときに発動するようにセット。
		・Thing2は、そのまんま。継承だけ追加し、返り値も継承

	*/
	def firstName2(id: Int): MyOption[String] = {
		Thing2("t")
	}
	def lastName2(id: Int): MyOption[String] = {
		Thing2("i")
	}

	def fullName9(id: Int): MyOption[String] = {
	  firstName2(id).bind(fnameIn => 
	  	lastName2(id).bind(lnameIn =>
				Thing2(fnameIn + lnameIn)
			)
	  )
	}

	val result = fullName9(0)

	//値の取り出し
	result match {
		case resu:Thing2[_] => println("result	"+resu.value)
		case _ => 
	}
	
	println("myFN	result	"+result)

	/*
		で、mySome はvalueを持っているので、取得できるよ、と。
		なるほどねー。

		ためしに2.9.2のOptionのコード読んだらまるっきり違っててびっくりしたんだけどどうすれば
	*/

}
new myFN