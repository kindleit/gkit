package gkit.mongo

case class Set[A]($set: A)
case class Unset[A]($unset: A)
case class Exists[A]($exists: A)
case class Push[A]($push: A)
case class Match[A]($match: A)
case class Lt[A]($lt: A)
case class Gt[A]($gt: A)
case class Lte[A]($lte: A)
case class Gte[A]($gte: A)
case class Ne[A]($ne: A)
case class Nin[A]($nin: A)
case class In[A]($in: A)
case class Project[A]($project: A)
case class Unwind[A]($unwind: A)
case class Group[A]($group: A)
case class Limit[A]($limit: A)
case class Skip[A]($skip: A)
case class Sort[A]($sort: A)
case class First[A]($first: A)
case class Last[A]($last: A)
case class AddToSet[A]($addToSet: A)
case class Max[A]($max: A)
case class Min[A]($min: A)
case class Avg[A]($avg: A)
case class Sum[A]($sum: A)
case class DayOfMonth[A]($dayOfMonth: A)
case class DayOfWeek[A]($dayOfWeek: A)
case class Year[A]($year: A)
case class Month[A]($month: A)
case class Week[A]($week: A)
case class Hour[A]($hour: A)
case class Minute[A]($minute: A)
case class Second[A]($second: A)
case class Millisecond[A]($millisecond: A)
case class Concat[A]($concat: A)
