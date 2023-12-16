package co.com.sura.postgres.maestros.repository;

import co.com.sura.postgres.maestros.data.HorarioTurnoData;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;


public interface HorarioTurnoRepository extends ReactiveCrudRepository<HorarioTurnoData,Integer> {

}
