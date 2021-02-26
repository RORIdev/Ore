package ore.data

import scala.language.higherKinds
import scala.collection.immutable
import ore.data.project.Dependency
import ore.db.{DbRef, Model, ModelService}
import ore.models.project.{TagColor, Version, VersionTag}
import enumeratum.values._

/**
  * The Platform a plugin/mod runs on
  *
  * @author phase
  */
sealed abstract class Platform(
    val value: Int,
    val name: String,
    val platformCategory: PlatformCategory,
    val priority: Int,
    val dependencyId: String,
    val tagColor: TagColor,
    val url: String
) extends IntEnumEntry {

  def createGhostTag(versionId: DbRef[Version], version: Option[String]): VersionTag =
    VersionTag(versionId, name, version, tagColor)
}
object Platform extends IntEnum[Platform] {

  val values: immutable.IndexedSeq[Platform] = findValues

  case object Nukkit
      extends Platform(
        0,
        "Cloudburst Nukkit",
        NukkitCategory,
        1,
        "!!IMPOSSIBLE!!",
        TagColor.Nukkit,
        "https://ci.opencollab.dev/job/NukkitX/job/Nukkit/job/master/"
      )

  case object PowerNukkit
      extends Platform(
        1,
        "PowerNukkit",
        NukkitCategory,
        0,
        "powernukkit",
        TagColor.PowerNukkit,
        "https://builds.powernukkit.org"
      )
/*
    case object SpongeVanilla
        extends Platform(
          3,
          "SpongeVanilla",
          NukkitCategory,
          2,
          "spongevanilla",
          TagColor.SpongeVanilla,
          "https://www.spongepowered.org/downloads/spongevanilla"
        )
  
    case object SpongeCommon
        extends Platform(
          4,
          "SpongeCommon",
          NukkitCategory,
          1,
          "sponge",
          TagColor.SpongeCommon,
          "https://www.spongepowered.org/downloads"
        )
  
    case object Lantern
        extends Platform(5, "Lantern", NukkitCategory, 2, "lantern", TagColor.Lantern, "https://www.lanternpowered.org/")
  
    case object Forge
        extends Platform(1, "Forge", ForgeCategory, 0, "forge", TagColor.PowerNukkit, "https://files.minecraftforge.net/")
   */

  def getPlatformsByDependencies(dependencies: Seq[Dependency]): Seq[Platform] = {
    val requiredDependencyIds = dependencies.filter(_.required).map(_.pluginId)
    val optionalDependencyIds = dependencies.filterNot(_.required).map(_.pluginId).filterNot(requiredDependencyIds.contains(_))
    
    if (requiredDependencyIds.contains(PowerNukkit.dependencyId)) {
      return Seq(PowerNukkit)
    }
    
    if (optionalDependencyIds.contains(PowerNukkit.dependencyId)) {
      return Seq(PowerNukkit, Nukkit)
    }
    
    Seq(Nukkit)
  }

  def ghostTags(versionId: DbRef[Version], dependencies: Seq[Dependency]): Seq[VersionTag] =
    getPlatformsByDependencies(dependencies)
      .map(p => p.createGhostTag(versionId, dependencies.find(_.pluginId == p.dependencyId).flatMap(_.version)))

  def createPlatformTags[F[_]](versionId: DbRef[Version], dependencies: Seq[Dependency])(
      implicit service: ModelService[F]
  ): F[Seq[Model[VersionTag]]] = service.bulkInsert(ghostTags(versionId, dependencies))

}

/**
  * The category of a platform.
  * Examples would be
  *
  * Sponge <- SpongeAPI, SpongeForge, SpongeVanilla
  * Forge <- Forge (maybe Rift if that doesn't die?)
  * Bukkit <- Bukkit, Spigot, Paper
  * Canary <- Canary, Neptune
  *
  * @author phase
  */
sealed trait PlatformCategory {
  def name: String
  def tagName: String

  def getPlatforms: Seq[Platform] = Platform.values.filter(_.platformCategory == this)
}

case object NukkitCategory extends PlatformCategory {
  val name    = "Nukkit Plugins"
  val tagName = "nukkit"
}

/*
case object ForgeCategory extends PlatformCategory {
  val name    = "Forge Mods"
  val tagName = "Forge"
}
*/

object PlatformCategory {
  def getPlatformCategories: Seq[PlatformCategory] = Seq(NukkitCategory/*, ForgeCategory*/)
}
