package co.com.sura.postgres.maestros.repository;

import co.com.sura.postgres.maestros.data.ProfesionData;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

public interface ProfesionRepository extends ReactiveCrudRepository<ProfesionData,Integer> {
}
