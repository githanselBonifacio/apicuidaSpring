package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;

public interface DiagnosticoRepository extends ReactiveCrudRepository<DiagnosticoData,String> {

    @Query("INSERT INTO public.diagnostico(codigo, nombre) VALUES ($1, $2);")
    Mono<Void> insertDiagnosticoQuery(
            @Param("$1") String codigo,
            @Param("$2") String nombre);

    default Mono<Void> insertDiagnostico(DiagnosticoData diagnosticoData){
        return insertDiagnosticoQuery(diagnosticoData.getCodigo(), diagnosticoData.getNombre());
    }
}
