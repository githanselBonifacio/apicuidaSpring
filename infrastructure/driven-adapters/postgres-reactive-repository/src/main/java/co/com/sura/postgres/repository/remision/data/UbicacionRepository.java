package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;


public interface UbicacionRepository extends ReactiveCrudRepository<UbicacionData,String> {

   @Override
   Mono<Boolean> existsById(String idUbicacion);

   @Query("DELETE FROM public.ubicaciones WHERE id_ubicacion=$1;")
   Mono<Void> deleteByIdUbicacion(String idUbicacion);

   @Query("INSERT INTO ubicaciones (" +
           " id_ubicacion," +
           " latitud," +
           " longitud," +
           " direccion," +
           " tipo_via," +
           " numero1," +
           " nro_interseccion," +
           " numero2," +
           " barrio," +
           " sin_nomenclatura," +
           " municipio," +
           " id_regional)" +
           " VALUES " +
           "($1, $2, $3, $4, $5, $6, $7, $8, $9, $10,$11,$12)"
   )
   Mono<Void> insertUbicacionQuery(
           @Param("$1") String idUbicacion,
           @Param("$2") double latitud,
           @Param("$3")  double longitud,
           @Param("$4")  String direccion,
           @Param("$5") String tipoVia,
           @Param("$6") String numero1,
           @Param("$7")  String numeroInterseccion,
           @Param("$8")  String numero2,
           @Param("$9")  String barrio,
           @Param("$10")  boolean sinNomenclatura,
           @Param("$11")  String municipio,
           @Param("$12") String idRegional
   );
   default Mono<Void> insertUbicacion(UbicacionData ubicacionData){
       return insertUbicacionQuery(
               ubicacionData.getIdUbicacion(),
               ubicacionData.getLatitud(),
               ubicacionData.getLongitud(),
               ubicacionData.getDireccion(),
               ubicacionData.getTipoVia(),
               ubicacionData.getNumero1(),
               ubicacionData.getNumeroInterseccion(),
               ubicacionData.getNumero2(),
               ubicacionData.getBarrio(),
               ubicacionData.getSinNomenclatura(),
               ubicacionData.getMunicipio(),
               ubicacionData.getIdRegional()
       );
   }
}
