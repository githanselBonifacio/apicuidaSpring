package co.com.sura.postgres.remision.repository.datospaciente;

import co.com.sura.postgres.remision.data.datospaciente.RegistroHistorialRemisionData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface RegistroHistorialRepository extends ReactiveCrudRepository<RegistroHistorialRemisionData,String> {

    @Query("SELECT COUNT(*) " +
            "FROM public.historial_remision " +
            "INNER JOIN public.remisiones ON public.remisiones.id_remision = public.historial_remision.id_remision " +
            "AND public.remisiones.id_regional = $2 " +
            "WHERE fecha_registro::date = $1")
    Mono<Integer> countByFechaNovedadRegional(LocalDate fechaRegistro, String idRegional);

    @Query("select * from  public.historial_remision WHERE id_remision = $1 ORDER BY fecha_registro DESC;")
    Flux<RegistroHistorialRemisionData> findAllByIdRemision(String idRemision);

}
