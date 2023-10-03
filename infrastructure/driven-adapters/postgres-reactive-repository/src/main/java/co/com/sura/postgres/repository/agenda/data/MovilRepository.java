package co.com.sura.postgres.repository.agenda.data;

import co.com.sura.entity.agenda.Movil;
import co.com.sura.entity.agenda.Profesional;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface MovilRepository extends ReactiveCrudRepository<MovilData,String> {
    Flux<MovilData> findByIdRegional(String idRegional);

   @Query("SELECT public.moviles.*  from moviles " +
           "LEFT JOIN public.conductores ON public.conductores.matricula_movil = public.moviles.matricula " +
           "WHERE public.conductores.numero_identificacion is null;")
   Flux<MovilData> findAllWithoutConductor();

   @Query("INSERT INTO public.moviles( " +
           " matricula, id_Regional, Marca, modelo) " +
           " VALUES ($1, $2, $3, $4);")
    Mono<Void> insertQueryMovil(
           @Param("$1") String matricula,
           @Param("$2") String idRegional,
           @Param("$3") String marca,
           @Param("$4") LocalDate modelo);

    default Mono<Void> insertMovil (Movil movil) {
        return insertQueryMovil(
                movil.getMatricula(),
                movil.getIdRegional(),
                movil.getMarca(),
                movil.getModelo()
        );
    }
}
