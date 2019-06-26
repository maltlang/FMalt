For($i=0;$i -lt $args.Count; $i++)
{
    Write-Host "Compiled file '$($args[$i])' ..."
}
dotnet ./fmalt.dll $args