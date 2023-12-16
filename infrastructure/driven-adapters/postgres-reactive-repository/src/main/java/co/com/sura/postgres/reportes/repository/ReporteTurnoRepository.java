package co.com.sura.postgres.reportes.repository;

import co.com.sura.postgres.reportes.data.ReporteTurnoData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface ReporteTurnoRepository extends ReactiveCrudRepository<ReporteTurnoData,Integer> {

    @Query("DELETE FROM reportes_turno WHERE fecha_turno = $1")
  Mono<Void> deleteByFechaturno(LocalDate fechaTurno);
}
